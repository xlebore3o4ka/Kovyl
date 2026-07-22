import sys
import json
import subprocess
import logging
import os
from urllib.parse import unquote, urlparse

# Определяем абсолютный путь к папке, где лежит этот скрипт
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
LOG_FILE_PATH = os.path.join(SCRIPT_DIR, 'kovyl-lsp.log')

# Настройка логирования с абсолютным путем
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(LOG_FILE_PATH, encoding='utf-8'),
        logging.StreamHandler(sys.stderr)
    ]
)
logger = logging.getLogger('kovyl-lsp')

# Фикс импорта для pygls 2.x+: класс теперь находится в lsp.server
from pygls.lsp.server import LanguageServer 
# Импортируем типы из lsprotocol (стандарт для pygls 2.x)
from lsprotocol.types import (
    TEXT_DOCUMENT_DID_OPEN,
    TEXT_DOCUMENT_DID_SAVE,
    Diagnostic,
    DiagnosticSeverity,
    Range,
    Position,
    PublishDiagnosticsParams
)

# Инициализируем сервер с обязательным указанием имени и версии
server = LanguageServer("RawKovyl-LSP", "0.1.0")
logger.info("Language Server initialized")

# Получаем путь к бинарнику линтера относительно папки скрипта
# Поднимаемся на 2 уровня вверх (utils -> src -> RawKovyl) и переходим в build/linter
LINTER_PATH = os.path.join(
    os.path.dirname(os.path.dirname(SCRIPT_DIR)),
    'build',
    'linter'
)
if sys.platform == 'win32':
    LINTER_PATH += '.exe'
logger.info(f"Linter path: {LINTER_PATH}")


def uri_to_path(uri):
    logger.debug(f"Converting URI to path: {uri}")
    path = unquote(urlparse(uri).path)
    result = path[1:] if sys.platform == 'win32' and path.startswith('/') else path
    logger.debug(f"Converted path: {result}")
    return result


def run_linter(file_path):
    logger.info(f"Running linter on: {file_path}")
    try:
        result = subprocess.run(
            [LINTER_PATH, file_path],  # Вызываем бинарник линтера
            capture_output=True,
            text=True,
            timeout=2
        )
        logger.debug(f"Linter stdout: {result.stdout[:200]}...")
        logger.debug(f"Linter stderr: {result.stderr[:200]}...")
        
        if result.stdout.strip():
            try:
                data = json.loads(result.stdout)
                logger.info(f"Linter returned {len(data)} errors")
                return data
            except json.JSONDecodeError as e:
                logger.error(f"Failed to parse JSON from linter: {e}")
                logger.error(f"Raw output: {result.stdout}")
                return None
        else:
            logger.warning("Linter returned empty output")
            return []
    except subprocess.TimeoutExpired:
        logger.error(f"Linter timeout (2s) for {file_path}")
        return None
    except FileNotFoundError:
        logger.error(f"Linter not found at: {LINTER_PATH}")
        return None
    except Exception as e:
        logger.error(f"Unexpected error in linter: {e}", exc_info=True)
        return None


def send_diagnostics(ls, uri, errors):
    logger.info(f"Sending diagnostics for {uri}, errors: {len(errors) if errors else 'None'}")
    
    if errors is None:
        logger.warning("Errors is None, skipping diagnostics")
        return

    diagnostics = []
    for i, e in enumerate(errors):
        logger.debug(f"Processing error {i}: {e}")
        diagnostics.append(Diagnostic(
            range=Range(
                start=Position(line=e.get('line', 0), character=e.get('column', 0)),
                end=Position(line=e.get('line', 0), character=e.get('column', 0) + e.get('len', 1))
            ),
            message=e.get('message', 'Error'),
            severity=DiagnosticSeverity.Error,
            code=f"{e.get('line', 0)}:{e.get('column', 0)}",
            source=e.get('file')
        ))

    logger.info(f"Publishing {len(diagnostics)} diagnostics")
    ls.text_document_publish_diagnostics(
        PublishDiagnosticsParams(uri=uri, diagnostics=diagnostics)
    )


def lint(ls, uri):
    logger.info(f"Lint requested for {uri}")
    file_path = uri_to_path(uri)
    
    if not file_path.endswith('.kvl'):
        logger.info(f"Skipping non-kvl file: {file_path}")
        return

    errors = run_linter(file_path)
    send_diagnostics(ls, uri, errors)
    logger.info(f"Lint completed for {uri}")


@server.feature(TEXT_DOCUMENT_DID_OPEN)
async def did_open(ls, params):
    logger.info(f"Document opened: {params.text_document.uri}")
    logger.info(f"ls type: {type(ls)}, dir: {[x for x in dir(ls) if 'diag' in x.lower()]}")
    lint(ls, params.text_document.uri)


@server.feature(TEXT_DOCUMENT_DID_SAVE)
async def did_save(ls, params):
    logger.info(f"Document saved: {params.text_document.uri}")
    lint(ls, params.text_document.uri)

if __name__ == "__main__":
    logger.info("Starting Language Server IO...")
    try:
        server.start_io()
    except Exception as e:
        logger.error(f"Server crashed: {e}", exc_info=True)
        raise