import os
import sys
import logging
import shutil

from colorstrings import colorStrings as cs
from get_data_from_archive_helpers import parse_args
from get_data_from_archive_helpers import get_newest_file

### Add logger
logger = logging.getLogger("Simple Descriptives");
logging.addLevelName(
    logging.INFO,    "\033[2;36m       \033[1;0m"
)

### Setup logger configuration
logging.basicConfig(
    stream=sys.stdout,
    level=logging.INFO
)

### Where am I? 
logger.info("{}[{}Copy/Paste newest file from archive{}]{}  --------- ".format(
    cs.RES,
    cs.PRGnBA,
    cs.RES,
    cs.PRGnBA,
))

### Read arguments
args = parse_args()
shutil.copyfile(get_newest_file(args), args.output_path)
