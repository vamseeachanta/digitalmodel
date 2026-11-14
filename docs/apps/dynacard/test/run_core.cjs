'use strict';
// The same pure UMD core is used by the page and this file-based test harness.
const fs = require('node:fs');
const core = require('../dynacard-core.js');

function execute(request) {
  try {
    if (request.operation === 'parse') return core.parseCardCsv(request.text);
    return core.analyse(request.card, request.well);
  } catch (error) {
    return {error: error.message};
  }
}

const request = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
process.stdout.write(JSON.stringify(Array.isArray(request) ? request.map(execute) : execute(request)));
