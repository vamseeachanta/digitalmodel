function exportToCsv(filename, rows) {
    var processRow = function (row) {
        var finalVal = '';
        for (var j = 0; j < row.length; j++) {
            var innerValue = row[j] === null ? '' : row[j].toString();
            if (row[j] instanceof Date) {
                innerValue = row[j].toLocaleString();
            };
            var result = innerValue.replace(/"/g, '""');
            if (result.search(/("|,|\n)/g) >= 0)
                result = '"' + result + '"';
            if (j > 0)
                finalVal += ',';
            finalVal += result;
        }
        return finalVal + '\n';
    };

    var csvFile = '';
    for (var i = 0; i < rows.length; i++) {
        csvFile += processRow(rows[i]);
    }

    var blob = new Blob([csvFile], { type: 'text/csv;charset=utf-8;' });
    if (navigator.msSaveBlob) { // IE 10+
        navigator.msSaveBlob(blob, filename);
    } else {
        var link = document.createElement("a");
        if (link.download !== undefined) { // feature detection
            // Browsers that support HTML5 download attribute
            var url = URL.createObjectURL(blob);
            link.setAttribute("href", url);
            link.setAttribute("download", filename);
            link.style.visibility = 'hidden';
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
        }
    }
}

downloadCSVFromJson = (filename, arrayOfJson, header=[]) => {
    // convert JSON to CSV
    const replacer = (key, value) => value === null ? '' : value // specify how you want to handle null values here
    if (Array.isArray(header) && header.length == 0) {
        header = Object.keys(arrayOfJson[0])
        header.sort();
    }
    let csv = arrayOfJson.map(row => header.map(fieldName =>
    JSON.stringify(row[fieldName], replacer)).join(','))
    csv.unshift(header.join(','))
    csv = csv.join('\r\n')

    // Create link and download
    try  {
        var link_csv = document.createElement('a');
        link_csv.setAttribute('href', 'data:text/csv;charset=utf-8,%EF%BB%BF' + encodeURIComponent(csv));
        link_csv.setAttribute('download', filename);
        link_csv.style.visibility = 'hidden';
        document.body.appendChild(link_csv);
        link_csv.click();
        document.body.removeChild(link_csv);
    }
    catch(err){
        window.alert("Download Failed. Try Google Chrome for Best results");
        return;
    }

};

//https://stackoverflow.com/questions/31757852/how-can-i-detect-internet-explorer-ie-and-microsoft-edge-using-javascript
function detectIE() {
  var ua = window.navigator.userAgent;

  // Test values; Uncomment to check result …

  // IE 10
  // ua = 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; Trident/6.0)';

  // IE 11
  // ua = 'Mozilla/5.0 (Windows NT 6.3; Trident/7.0; rv:11.0) like Gecko';

  // Edge 12 (Spartan)
  // ua = 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.71 Safari/537.36 Edge/12.0';

  // Edge 13
  // ua = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2486.0 Safari/537.36 Edge/13.10586';

  var msie = ua.indexOf('MSIE ');
  if (msie > 0) {
    // IE 10 or older => return version number
    return parseInt(ua.substring(msie + 5, ua.indexOf('.', msie)), 10);
  }

  var trident = ua.indexOf('Trident/');
  if (trident > 0) {
    // IE 11 => return version number
    var rv = ua.indexOf('rv:');
    return parseInt(ua.substring(rv + 3, ua.indexOf('.', rv)), 10);
  }

  var edge = ua.indexOf('Edge/');
  if (edge > 0) {
    // Edge (IE 12+) => return version number
    return parseInt(ua.substring(edge + 5, ua.indexOf('.', edge)), 10);
  }

  // other browser
  return false;
}

// https://stackoverflow.com/questions/2592092/executing-script-elements-inserted-with-innerhtml
function insertAndExecuteInnerHTML(id, text)
{
    domelement = document.getElementById(id);
    domelement.innerHTML = text;
    var scripts = [];

    ret = domelement.childNodes;
    for ( var i = 0; ret[i]; i++ ) {
      if ( scripts && nodeName( ret[i], "script" ) && (!ret[i].type || ret[i].type.toLowerCase() === "text/javascript") ) {
            scripts.push( ret[i].parentNode ? ret[i].parentNode.removeChild( ret[i] ) : ret[i] );
        }
    }

    for(script in scripts)
    {
      evalScript(scripts[script]);
    }
    }
    function nodeName( elem, name ) {
    return elem.nodeName && elem.nodeName.toUpperCase() === name.toUpperCase();
    }
    function evalScript( elem ) {
    data = ( elem.text || elem.textContent || elem.innerHTML || "" );

    var head = document.getElementsByTagName("head")[0] || document.documentElement,
    script = document.createElement("script");
    script.type = "text/javascript";
    script.appendChild( document.createTextNode( data ) );
    head.insertBefore( script, head.firstChild );
    head.removeChild( script );

    if ( elem.parentNode ) {
        elem.parentNode.removeChild( elem );
    }
}
