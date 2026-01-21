function addChartToDOM(chartData) {
    chartData.dom = chartData.label;
    chartData = parseJSONData(chartData);
    chartData = dataTransformation(chartData)
    chartData = assessChartDataForChartSettings(chartData);
    chartData.colorsCategory10 = ["#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf"]
    chartData.colorsPaired = ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99","#b15928"]
    if (chartData.chartType == "scatter") { scatterChartD02(chartData); };
    // if (chartData.chartType == "column") { barChartD02(dom, chartData, options); };
    if (chartData.chartType == "line") { lineChartD02(chartData); };

}


function lineChartD02(chartData) {
    var width = chartData.options.width || 800;
    var height = chartData.options.height || 600;
    var displayAxes = chartData.options.displayAxes || true;
    var displayTitle = chartData.options.displayTitle || true;
    var clipPath = chartData.options.clipPath || false;
    var padding = chartData.options.padding || 80;

    var svg = d3.select(document.getElementById(chartData.dom))
        .append('svg')
        .attr('height', height)
        .attr('width', width);

    // Scaling Axes
    xScale = d3.scaleLinear()
        .domain([chartData.options.xRange.min, chartData.options.xRange.max])
        .range([padding, width - padding*2]);
    yScale = d3.scaleLinear()
        .domain([chartData.options.yRange.min, chartData.options.yRange.max])
        .range([height - padding, padding]);


    XYPaircounter = 0
    chartData.dataSources.forEach(function (arrayItem) {
        dataSourceData = arrayItem.data;
        arrayItem.plotSets.forEach(function (arrayItem){
            XYPaircounter = XYPaircounter + 1;
            plotSet = arrayItem;
            plotSet.dom = chartData.dom;
            plotSet.marker.XYPaircounter = XYPaircounter;
            plotSet.marker.label = "marker" + String(XYPaircounter);
            plotSet.legendFlag = chartData.options.legend.flag;
            plotSet.legendX = width - 2 * padding;
            plotSet.legendY = 3 * padding + 30*(XYPaircounter-1);
            plotSet.marker.fillColor = plotSet.marker.fillColor || chartData.colorsPaired[XYPaircounter*2+1];
            plotSet.marker.mouseover.fillColor = plotSet.marker.mouseover.fillColor || chartData.colorsPaired[XYPaircounter*2];
            addLine(svg, xScale, yScale, dataSourceData, plotSet);
            addMakers(svg, xScale, yScale, dataSourceData, plotSet);
            });
        });
            
    // Preparing Axes
    var xAxis = d3.axisBottom()
        .scale(xScale)
        .ticks(5);
    var yAxis = d3.axisLeft()
        .scale(yScale)
        .ticks(5);

    if (displayTitle == true) {
        svg.append("text")
            .attr("x", (width/2))
            .attr("y", (padding/2))
            .attr("text-anchor", "middle")
            .style("font-size", "24px") 
            .text(chartData.options.title);
        }    

    if (displayAxes == true) {
        svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(0," + (height-padding) + ")")
            .call(xAxis);
        // X Axis Label
        svg.append("text")             
            .attr("x", width/2)
            .attr("y", height - padding/2 + 20)
            .style("text-anchor", "middle")
            .style("font-size", "20px") 
            .text(chartData.options.XLabel);

        svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(" + padding + ",0)")
            .call(yAxis); 
        // Y Axis Label
        svg.append("text")
            .attr("transform", "rotate(-90)")
            .attr("x", -height/2)
            .attr("y", padding/4)
            // .attr("dy", "1em")
            .style("text-anchor", "middle")
            .style("font-size", "20px") 
            .text(chartData.options.YLabel);

        
        }

    if (clipPath == true) {
        // clip Path code is not working
        svg.append("clipPath")
            .attr("id", "chart-area")
            .append("rect")
            .attr("x", padding)
            .attr("y", padding)
            .attr("width", width - padding*3)
            .attr("height", height - padding*2);

        svg.append("g")
            .attr("id", "circles")
            .attr("clip-path", "url(#chart-area)");
        
        }

}


function parseJSONData(chartData) {
    chartData.dataSources.forEach(function (arrayItem) {
        arrayItem.data = JSON.parse(arrayItem.data);
        });
    return chartData;
}

function assessChartDataForChartSettings(chartData) {
    var dataSourcexRangeArray  = [];
    var dataSourceyRangeArray  = [];
    chartData.dataSources.forEach(function (arrayItem) {
        data = arrayItem.data;
        var xRangeArray = [];
        var yRangeArray = [];
        arrayItem.plotSets.forEach(function (arrayItem){
            xRangeArray.push(findMinMaxJson(data, arrayItem.x));
            yRangeArray.push(findMinMaxJson(data, arrayItem.y));
            var AColumn = arrayItem.marker.sizeColumn || arrayItem.marker.y;
            var aRange = findMinMaxJson(data, AColumn);
            arrayItem.aRange = aRange;
            });
        arrayItem.xRange = {'min': findMinMaxJson(xRangeArray, 'min').min, 'max':  findMinMaxJson(xRangeArray, 'max').max};
        dataSourcexRangeArray.push(arrayItem.xRange);
        arrayItem.yRange = {'min': findMinMaxJson(yRangeArray, 'min').min, 'max':  findMinMaxJson(yRangeArray, 'max').max};
        dataSourceyRangeArray.push(arrayItem.yRange);
        });

    dataSourceXMin = findMinMaxJson(dataSourcexRangeArray, 'min').min
    dataSourceXMax = findMinMaxJson(dataSourcexRangeArray, 'max').max
    dataSourceYMin = findMinMaxJson(dataSourceyRangeArray, 'min').min
    dataSourceYMax = findMinMaxJson(dataSourceyRangeArray, 'max').max
    var XExtent = dataSourceXMax - dataSourceXMin;
    var YExtent = dataSourceYMax - dataSourceYMin;
    dataSourcexRange = {'min': dataSourceXMin - 0.1*XExtent, 'max':  dataSourceXMax + 0.1*XExtent};
    dataSourceyRange = {'min': dataSourceYMin - 0.1*YExtent, 'max':  dataSourceYMax + 0.1*YExtent};
    chartData.options.xRange = chartData.options.xRange || dataSourcexRange;
    chartData.options.yRange = chartData.options.yRange || dataSourceyRange;

    if (chartData.options.equalAxes == true) {
        var XExtent = chartData.options.xRange['max'] - chartData.options.xRange['min'];
        var YExtent = chartData.options.yRange['max'] - chartData.options.yRange['min'];
            if (XExtent > YExtent) {
            var XAdjustment = 0;
            var YAdjustment = (XExtent - YExtent)/2;
        }
        else {
            var XAdjustment = (YExtent - XExtent)/2;
            var YAdjustment = 0;
        }
    }
    else {
        var XAdjustment = 0;
        var YAdjustment = 0;
    }

    chartData.options.xRange.min = chartData.options.xRange.min - XAdjustment
    chartData.options.xRange.max = chartData.options.xRange.max + XAdjustment
    chartData.options.yRange.min = chartData.options.yRange.min - YAdjustment
    chartData.options.yRange.max = chartData.options.yRange.max + YAdjustment

    return chartData;
    
}

function dataTransformation(chartData) {
    if (chartData.options.dataConversion.flag) {
        dataset.forEach(function (arrayItem) {
            arrayItem.dateTime = new Date(arrayItem.year, (arrayItem.month-1));
            });
    }

    return chartData;
}

function scatterChartD02(chartData) {
    var width = chartData.options.width || 800;
    var height = chartData.options.height || 600;
    var displayAxes = chartData.options.displayAxes || true;
    var displayTitle = chartData.options.displayTitle || true;
    var clipPath = chartData.options.clipPath || true;
    var padding = chartData.options.padding || 80;

    var svg = d3.select(document.getElementById(chartData.dom))
        .append('svg')
        .attr('height', height)
        .attr('width', width);

    // Scaling Axes
    xScale = d3.scaleLinear()
        .domain([chartData.options.xRange.min, chartData.options.xRange.max])
        .range([padding, width - padding*2]);
    yScale = d3.scaleLinear()
        .domain([chartData.options.yRange.min, chartData.options.yRange.max])
        .range([height - padding, padding]);

    XYPaircounter = 0
    chartData.dataSources.forEach(function (arrayItem) {
        dataSourceData = arrayItem.data;
        arrayItem.plotSets.forEach(function (arrayItem){
            XYPaircounter = XYPaircounter + 1;
            plotSet = arrayItem;
            plotSet.dom = chartData.dom;
            plotSet.marker.XYPaircounter = XYPaircounter;
            plotSet.marker.label = "marker" + String(XYPaircounter);
            plotSet.legendFlag = chartData.options.legend.flag;
            plotSet.legendX = width - 2 * padding;
            plotSet.legendY = 3 * padding + 30*(XYPaircounter-1);
            plotSet.marker.fillColor = plotSet.marker.fillColor || chartData.colorsPaired[XYPaircounter*2+1];
            plotSet.marker.mouseover.fillColor = plotSet.marker.mouseover.fillColor || chartData.colorsPaired[XYPaircounter*2];
            addMakers(svg, xScale, yScale, dataSourceData, plotSet);
            });
        });
    
    // Preparing Axes
    var xAxis = d3.axisBottom()
        .scale(xScale)
        .ticks(5);
    var yAxis = d3.axisLeft()
        .scale(yScale)
        .ticks(5);

    if (displayTitle == true) {
        svg.append("text")
            .attr("x", (width/2))
            .attr("y", (padding/2))
            .attr("text-anchor", "middle")
            .style("font-size", "24px") 
            .text(chartData.options.title);
        }    

    if (displayAxes == true) {
        svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(0," + (height-padding) + ")")
            .call(xAxis);
        // X Axis Label
        svg.append("text")             
            .attr("x", width/2)
            .attr("y", height - padding/2 + 20)
            .style("text-anchor", "middle")
            .style("font-size", "20px") 
            .text(chartData.options.XLabel);

        svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(" + padding + ",0)")
            .call(yAxis); 
        // Y Axis Label
        svg.append("text")
            .attr("transform", "rotate(-90)")
            .attr("x", -height/2)
            .attr("y", padding/4)
            // .attr("dy", "1em")
            .style("text-anchor", "middle")
            .style("font-size", "20px") 
            .text(chartData.options.YLabel);

        
        }

    if (clipPath == true) {
        // clip Path code is not working
        svg.append("clipPath")
            .attr("id", "chart-area")
            .append("rect")
            .attr("x", padding)
            .attr("y", padding)
            .attr("width", width - padding*3)
            .attr("height", height - padding*2);

        svg.append("g")
            .attr("id", "circles")
            .attr("clip-path", "url(#chart-area)");
        
        }

}

function addMakers(svg, xScale, yScale, data, plotSet) {
    // Scaling Makers
    plotSet.marker.sizeRange = plotSet.marker.sizeRange || [4, 10];
    aScale = d3.scaleSqrt()
        .domain([0, plotSet.aRange.max])
        .range(plotSet.marker.sizeRange);

    svg.selectAll(plotSet.marker.label)
    .data(data)
    .enter()
    .append(plotSet.marker.shape)
    .attr("cx", function(d) {return xScale(d[plotSet.x]);})
    .attr("cy", function(d) {return yScale(d[plotSet.y]);})
    .attr("r", function(d) {
        if (plotSet.marker.size != 'variable') {return plotSet.marker.size;} 
        else {return aScale(d[plotSet.marker.sizeColumn]);}})
    .attr("fill", plotSet.marker.fillColor)

    // .on("mouseover", function(d) {
    //     d3.select(this)
    //         .attr("fill", plotSet.marker.mouseover.fillColor);
    //     var xPosition = parseFloat(d3.select(this).attr("cx")) + 20;
    //     var yPosition = parseFloat(d3.select(this).attr("cy")) - 20;
    //     mouseoverJSON = {};
    //     plotSet.marker.mouseover.columns.forEach(function (arrayItem) {
    //         mouseoverJSON[arrayItem] = d[arrayItem]; });
    //     mouseoverJSONString = JSON.stringify(mouseoverJSON, undefined, 2).replace("{", "").replace("}", "");
    //     svg.append("text")
    //     .attr("id", "tooltip")
    //     .attr("x", xPosition)
    //     .attr("y", yPosition)
    //     .attr("text-anchor", "left")
    //     .attr("font-family", "sans-serif")
    //     .attr("font-size", "13px")
    //     .attr("font-weight", "bold")
    //     .attr("fill", "black")
    //     .text(mouseoverJSONString);
    //     })
    // .on("mouseout", function(d) {
    //     d3.select(this)
    //         .transition()
    //         .duration(250)
    //         .attr("fill", plotSet.marker.fillColor);
    //     d3.select("#tooltip").remove(); });

    .append("title")
    .text(function(d) { 
        mouseoverJSON = {};
        mouseoverColumns = plotSet.marker.mouseover.columns || [plotSet.y];
        mouseoverColumns.forEach(function (arrayItem) {
        mouseoverJSON[arrayItem] = d[arrayItem]; });
        return JSON.stringify(mouseoverJSON, undefined, 2).replace("{", "").replace("}", "");
    });

    if (plotSet.dataLabel.flag == true) {
        svg.selectAll("text")
            .data(data)
            .enter()
            .append("text")
            .text(function(d) {
                if (plotSet.dataLabel.flag == true) { return d[plotSet.dataLabel.column]; }
                else { return ( "(" + d[xColumn] + ', ' + d[yColumn] + ")"); } })
            .attr("x", function(d) {return xScale(d[plotSet.x]) + 10;})
            .attr("y", function(d) {return yScale(d[plotSet.y]) - 10;})
            .attr("font-family", "sans-serif")
            .attr("font-size", "14px")
            .attr("fill", plotSet.marker.fillColor); 
        }

    if (plotSet.legendFlag == true) {
        svg.append(plotSet.marker.shape)
            .attr("cx", plotSet.legendX)
            .attr("cy", plotSet.legendY)
            .attr("r", function() {
                if (plotSet.marker.size != 'variable') {return plotSet.marker.size;} 
                else {return (plotSet.marker.sizeRange[0] + plotSet.marker.sizeRange[1])/2;}})
            .attr("fill", plotSet.marker.fillColor);
        svg.append("text")
            .attr("x", plotSet.legendX + 15)
            .attr("y", plotSet.legendY)
            .text(plotSet.legend)
            .style("font-size", "15px")
            .attr("alignment-baseline","middle");

        }

}


function addLine(svg, xScale, yScale, data, plotSet) {

    var d3line = d3.line()
                .x(function(d) { return Math.round(xScale(d[plotSet.x])); })
                .y(function(d) { return Math.round(yScale(d[plotSet.y])); });

    svg.append("path")
        .datum(data)
        .attr("fill", "none")
        // .attr("stroke", plotSet.marker.fillColor+" !important")
        .attr("stroke", plotSet.marker.fillColor)
        .attr("stroke-width", 1.5)
        .attr("d", d3line);

}

// Bar chart with options
function barChartD02(dom, dataset, options) {
    var width = options.width || 600;
    var height = options.height || 400;
    var barPadding = options.barPadding || 1;
    var fillColor = options.fillColor || 'steelblue';
    var textMargin = options.textMargin || 0.1;
    var maxDataValue = d3.max(dataset);

    if (options.chartType == 'column') {
        var barSpacing = width / dataset.length;
        var dataBarWidth = barSpacing - barPadding;
        var dataBarScale = height/maxDataValue * ( 1- textMargin);
        var dataBarMaximum = height; }

    if (options.chartType == 'bar') {
        var barSpacing =  height / dataset.length;
        var dataBarWidth = barSpacing - barPadding;
        var dataBarScale = width / maxDataValue * ( 1- textMargin);
        var dataBarMaximum = width; }

    var svg = d3.select(document.getElementById(dom))
        .append('svg')
        .attr('height', height)
        .attr('width', width);

    svg.selectAll("rect")
        .data(dataset)
        .enter()
        .append("rect")
        .attr("x", function(d, i) {return i * barSpacing;})
        .attr("y", function(d) {return (dataBarMaximum - d * dataBarScale);})
        .attr("width", dataBarWidth)
        .attr("height", function(d) {return (d * dataBarScale);})
        .attr("fill", fillColor);

    if (options.dataLabel == true) {
    svg.selectAll("text")
        .data(dataset)
        .enter()
        .append("text")
        .text(function(d) {return d;})
        .attr("x", function(d, i) {return i * barSpacing + (barSpacing-barPadding) / 2;})
        .attr("y", function(d) {return (dataBarMaximum - d * dataBarScale) - 14;})
        .attr("font-family", "sans-serif")
        .attr("font-size", "14px"); }

}


function barChart_D01(dom, data, options) {
    var margin = {top: 30, right: 20, bottom: 30, left: 50}
    var width = options.width || 800;
    var height = options.height || 200;
    var barPadding = options.barPadding || 1;
    var fillColor = options.fillColor || 'steelblue';

    var barSpacing = height / data.length;
    var barHeight = barSpacing - barPadding;
    var maxValue = d3.max(data);
    var widthScale = width / maxValue;

    var svg = d3.select(dom).append('svg')
            .attr('height', height)
            .attr('width', width)
            .selectAll('rect')
            .data(data)
            .enter()
            .append('rect')
            .attr('y', function (d, i) { return i * barSpacing })
            .attr('height', barHeight)
            .attr('x', 0)
            .attr('width', function (d) { return d*widthScale})
            .style('fill', fillColor);
}

// basicD3 bar chart
function simpleBarChartD01(dom, dataset) {
    d3.select(document.getElementById(dom))
        .selectAll("div")
        .data(dataset)
        .enter()
        .append("div")
        .attr('class', 'bar')
        .style('height', function(d) {return d*5 + 'px'});
}

// basicD3 playground
function basicD3PlayGround(dom, dataset) {
    d3.select(document.getElementById(dom))
        .selectAll("p")
        .data(dataset)
        .enter()
        .append("p")
        .text(function(d) { return d;})
        .text(function(d) { return "I can print element " + d;})
        .style('color', function(d) {if (d>15) {return 'red';} else {return 'black';}});
}

// Using Mike Bostock's Towards Reusable Charts Pattern
function barChart_Advanced() {

    // All options that should be accessible to caller
	var data = [];
    var width = 900;
    var height = 200;
    var xAxisLabel = 'x';
    var yAxisLabel = 'y';
    var barPadding = 1;
    var fillColor = 'steelblue';

    function chart(selection){
        selection.each(function (data) {
            var barSpacing = height / data.length;
            var barHeight = barSpacing - barPadding;
            var maxValue = d3.max(data);
            var widthScale = width / maxValue;
            // var yScale = d3.scaleLinear.([height, 0]);

            d3.select(this).append('svg')
                .attr('height', height)
                .attr('width', width)
                .selectAll('rect')
                .data(data)
                .enter()
                .append('rect')
                .attr('y', function (d, i) { return i * barSpacing })
                .attr('height', barHeight)
                .attr('x', 0)
                .attr('width', function (d) { return d*widthScale})
                .style('fill', fillColor);
            
            updateWidth = function() {
            widthScale = width / maxValue;
            bars.transition().duration(1000).attr('width', function(d) { return d*widthScale});
            svg.transition().duration(1000).attr('width', width);
            };

            updateData = function() {
            svg.transition().duration(1000).attr('data', data);
            };

        });
    }

    chart.data = function(value) {
        if (!arguments.length) return data;
        data = value;
        if (typeof updateData === 'function') updateData();
        return chart;
    };

    chart.width = function(value) {
        if (!arguments.length) return margin;
        width = value;
        if (typeof updateWidth === 'function') updateWidth();
        return chart;
    };

    chart.height = function(value) {
        if (!arguments.length) return height;
        height = value;
        return chart;
    };

    chart.barPadding = function(value) {
        if (!arguments.length) return barPadding;
        barPadding = value;
        return chart;
    };

    chart.fillColor = function(value) {
        if (!arguments.length) return fillColor;
        fillColor = value;
        return chart;
    };

    return chart;
}


function barChartWithTransitions() {

    // All options that should be accessible to caller
	var data = [];
    var width = 900;
    var height = 200;
    var xAxisLabel = 'x';
    var yAxisLabel = 'y';
    var barPadding = 1;
    var fillColor = 'steelblue';
	var updateData;
	var updateWidth;

    function chart(selection){
        selection.each(function (data) {
            var barSpacing = height / data.length;
            var barHeight = barSpacing - barPadding;
            var maxValue = d3.max(data);
            var widthScale = width / maxValue;

            d3.select(this).append('svg')
                .attr('height', height)
                .attr('width', width)
                .selectAll('rect')
                .attr('data', data)
                .enter()
                .append('rect')
                .attr('y', function (d, i) { return i * barSpacing })
                .attr('height', barHeight)
                .attr('x', 0)
                .attr('width', function (d) { return d*widthScale})
                .style('fill', fillColor);
            
            updateWidth = function() {
            widthScale = width / maxValue;
            bars.transition().duration(1000).attr('width', function(d) { return d*widthScale});
            svg.transition().duration(1000).attr('width', width);
            };

            updateData = function() {
            bars.transition().duration(1000).attr('data', data);
            };

        });
    }

    chart.data = function(value) {
        if (!arguments.length) return data;
        data = value;
        if (typeof updateData === 'function') updateData();
        return chart;
    };

    chart.width = function(value) {
        if (!arguments.length) return margin;
        width = value;
        if (typeof updateWidth === 'function') updateWidth();
        return chart;
    };

    chart.height = function(value) {
        if (!arguments.length) return height;
        height = value;
        return chart;
    };

    chart.barPadding = function(value) {
        if (!arguments.length) return barPadding;
        barPadding = value;
        return chart;
    };

    chart.fillColor = function(value) {
        if (!arguments.length) return fillColor;
        fillColor = value;
        return chart;
    };

    return chart;
}


function findMinMaxJson(arr, prop) {
    let min = arr[0][prop], max = arr[0][prop];

    for (let i = 1, len=arr.length; i < len; i++) {
        let v = arr[i][prop];
        min = (v < min) ? v : min;
        max = (v > max) ? v : max;
      }

    result = {min: min, max: max}

    return result;
}


function lineChartD01(dom, dataset) {
    var width = chartData.options.width || 800;
    var height = chartData.options.height || 600;
    var displayAxes = chartData.options.displayAxes || true;
    var displayTitle = chartData.options.displayTitle || true;
    var clipPath = chartData.options.clipPath || false;
    var padding = chartData.options.padding || 80;

    var svg = d3.select(document.getElementById(dom))
        .append('svg')
        .attr('height', height)
        .attr('width', width);

    // Scaling Axes
    xScale = d3.scaleLinear()
        .domain([chartData.options.xRange.min, chartData.options.xRange.max])
        .range([padding, width - padding*2]);
    yScale = d3.scaleLinear()
        .domain([chartData.options.yRange.min, chartData.options.yRange.max])
        .range([height - padding, padding]);

    xColumn = x[0]
    yColumn = y[0]
    // Scaling Data
    var resultXMinMax = findMinMaxJson(dataset, xColumn);
    if (options.xDate ) {
        var xScale = d3.scaleTime()
            .domain([resultXMinMax['min'], resultXMinMax['max']])
            .range([padding, width - padding*2]);
    }
    else {
        var xScale = d3.scaleLinear()
            .domain([resultXMinMax['min'], resultXMinMax['max']])
            .range([padding, width - padding*2]);
    }

    var resultYMinMax = findMinMaxJson(dataset, yColumn);
    var yScale = d3.scaleLinear()
        .domain([resultYMinMax['min'], resultYMinMax['max']])
        .range([height - padding, padding]);

    var aScale = d3.scaleSqrt()
        // .domain([resultYMinMax['min'], resultYMinMax['max']])
        .domain([0, resultYMinMax['max']])
        .range([0, 10]);

    // Preparing Axes
    var xAxis = d3.axisBottom()
        .scale(xScale)
        .ticks(5);
    var yAxis = d3.axisLeft()
        .scale(yScale)
        .ticks(5);

    // Define line generator
    var line = d3.line()
                .x(function(d) { return xScale(d[xColumn]); })
                .y(function(d) { return yScale(d[yColumn]); });
    svg.append("path")
        .datum(dataset)
        .attr("class", "line")
        .attr("d", line);

    svg.selectAll("circle")
        .data(dataset)
        .enter()
        .append("circle")
        .attr("cx", function(d) {return xScale(d[xColumn]);})
        .attr("cy", function(d) {return yScale(d[yColumn]);})
        .attr("r", function(d) {
            if (options.marker.size != 'variable') {return options.marker.size;} 
            else {return aScale(d[yColumn]);}})
        .attr("fill", options.marker.fillColor)
        .on("mouseover", function(d) {
            d3.select(this)
                .attr("fill", "orange");})
        .on("mouseout", function(d) {
            d3.select(this)
                .transition()
                .duration(250)
                .attr("fill", options.marker.fillColor)})
        .append("title")
        .text(function(d) {return JSON.stringify(d, undefined, 2).replace("{", "").replace("}", "");});

    if (options.dataLabel.flag == true) {
        svg.selectAll("text")
            .data(dataset)
            .enter()
            .append("text")
            .text(function(d) {return ( "(" + d[xColumn] + ', ' + d[yColumn] + ")");})
            .attr("x", function(d) {return xScale(d[xColumn]);})
            .attr("y", function(d) {return yScale(d[yColumn]);})
            .attr("font-family", "sans-serif")
            .attr("font-size", "14px")
            .attr("fill", "red"); }

    if (displayAxes == true) {
        svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(0," + (height-padding) + ")")
            .call(xAxis);
        svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(" + padding + ",0)")
            .call(yAxis); }

    if (clipPath == true) {
        // clip Path code is not working
        svg.append("clipPath")
            .attr("id", "chart-area")
            .append("rect")
            .attr("x", padding)
            .attr("y", padding)
            .attr("width", width - padding*3)
            .attr("height", height - padding*2);

        svg.append("g")
            .attr("id", "circles")
            .attr("clip-path", "url(#chart-area)");
        
    }

}
