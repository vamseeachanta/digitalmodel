import React, { useEffect, useRef, useState } from 'react';
import * as d3 from 'd3';
import { Box, Paper, Typography, CircularProgress } from '@mui/material';

interface PolarPlotProps {
  data: {
    headings: number[];
    values: number[];
    unit: string;
    title?: string;
  };
  width?: number;
  height?: number;
}

const PolarPlot: React.FC<PolarPlotProps> = ({ 
  data, 
  width = 600, 
  height = 600 
}) => {
  const svgRef = useRef<SVGSVGElement>(null);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    if (!data || !data.headings || !data.values || !svgRef.current) return;

    setLoading(true);
    drawPolarPlot();
    setLoading(false);
  }, [data, width, height]);

  const drawPolarPlot = () => {
    const svg = d3.select(svgRef.current);
    svg.selectAll('*').remove();

    const margin = { top: 50, right: 50, bottom: 50, left: 50 };
    const plotWidth = width - margin.left - margin.right;
    const plotHeight = height - margin.top - margin.bottom;
    const radius = Math.min(plotWidth, plotHeight) / 2;

    const g = svg
      .append('g')
      .attr('transform', `translate(${width / 2},${height / 2})`);

    // Scales
    const angleScale = d3.scaleLinear()
      .domain([0, 360])
      .range([0, 2 * Math.PI]);

    const radiusScale = d3.scaleLinear()
      .domain([0, Math.max(...data.values) * 1.1])
      .range([0, radius]);

    // Grid circles
    const gridLevels = 5;
    const gridValues = d3.range(0, gridLevels + 1).map(
      d => (Math.max(...data.values) * d) / gridLevels
    );

    // Draw grid circles
    g.selectAll('.grid-circle')
      .data(gridValues)
      .enter()
      .append('circle')
      .attr('class', 'grid-circle')
      .attr('r', d => radiusScale(d))
      .style('fill', 'none')
      .style('stroke', '#ccc')
      .style('stroke-dasharray', '2,2');

    // Draw grid lines
    const angles = d3.range(0, 360, 15);
    g.selectAll('.grid-line')
      .data(angles)
      .enter()
      .append('line')
      .attr('class', 'grid-line')
      .attr('x1', 0)
      .attr('y1', 0)
      .attr('x2', d => radius * Math.cos((d - 90) * Math.PI / 180))
      .attr('y2', d => radius * Math.sin((d - 90) * Math.PI / 180))
      .style('stroke', '#e0e0e0')
      .style('stroke-width', 1);

    // Draw angle labels
    g.selectAll('.angle-label')
      .data(angles)
      .enter()
      .append('text')
      .attr('class', 'angle-label')
      .attr('x', d => (radius + 20) * Math.cos((d - 90) * Math.PI / 180))
      .attr('y', d => (radius + 20) * Math.sin((d - 90) * Math.PI / 180))
      .attr('text-anchor', 'middle')
      .attr('dy', '0.35em')
      .style('font-size', '12px')
      .text(d => `${d}°`);

    // Draw radius labels
    g.selectAll('.radius-label')
      .data(gridValues.slice(1))
      .enter()
      .append('text')
      .attr('class', 'radius-label')
      .attr('x', 5)
      .attr('y', d => -radiusScale(d))
      .attr('dy', '0.35em')
      .style('font-size', '10px')
      .style('fill', '#666')
      .text(d => d.toFixed(1));

    // Prepare data for line
    const lineData = data.headings.map((heading, i) => ({
      angle: heading,
      value: data.values[i]
    }));

    // Add first point at end to close the shape
    lineData.push(lineData[0]);

    // Line generator
    const line = d3.lineRadial<any>()
      .angle(d => angleScale(d.angle))
      .radius(d => radiusScale(d.value))
      .curve(d3.curveLinearClosed);

    // Draw the data area
    g.append('path')
      .datum(lineData)
      .attr('class', 'data-area')
      .attr('d', line)
      .style('fill', 'rgba(33, 150, 243, 0.3)')
      .style('stroke', '#2196F3')
      .style('stroke-width', 2);

    // Draw data points
    g.selectAll('.data-point')
      .data(lineData.slice(0, -1))
      .enter()
      .append('circle')
      .attr('class', 'data-point')
      .attr('cx', d => radiusScale(d.value) * Math.cos(angleScale(d.angle) - Math.PI / 2))
      .attr('cy', d => radiusScale(d.value) * Math.sin(angleScale(d.angle) - Math.PI / 2))
      .attr('r', 4)
      .style('fill', '#2196F3')
      .style('stroke', '#fff')
      .style('stroke-width', 2)
      .style('cursor', 'pointer')
      .on('mouseover', function(event, d) {
        // Tooltip
        const tooltip = d3.select('body')
          .append('div')
          .attr('class', 'polar-tooltip')
          .style('position', 'absolute')
          .style('padding', '10px')
          .style('background', 'rgba(0, 0, 0, 0.8)')
          .style('color', 'white')
          .style('border-radius', '4px')
          .style('font-size', '12px')
          .style('pointer-events', 'none')
          .style('opacity', 0);

        tooltip.transition()
          .duration(200)
          .style('opacity', 1);

        tooltip.html(`
          <strong>Heading:</strong> ${d.angle}°<br/>
          <strong>Value:</strong> ${d.value.toFixed(2)} ${data.unit}
        `)
          .style('left', (event.pageX + 10) + 'px')
          .style('top', (event.pageY - 10) + 'px');

        d3.select(this)
          .transition()
          .duration(100)
          .attr('r', 6);
      })
      .on('mouseout', function() {
        d3.selectAll('.polar-tooltip').remove();
        d3.select(this)
          .transition()
          .duration(100)
          .attr('r', 4);
      });

    // Title
    if (data.title) {
      svg.append('text')
        .attr('x', width / 2)
        .attr('y', 20)
        .attr('text-anchor', 'middle')
        .style('font-size', '16px')
        .style('font-weight', 'bold')
        .text(data.title);
    }

    // Unit label
    svg.append('text')
      .attr('x', width / 2)
      .attr('y', height - 10)
      .attr('text-anchor', 'middle')
      .style('font-size', '12px')
      .style('fill', '#666')
      .text(`Units: ${data.unit}`);
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" height={height}>
        <CircularProgress />
      </Box>
    );
  }

  return (
    <Paper elevation={2} sx={{ p: 2 }}>
      <svg ref={svgRef} width={width} height={height} />
    </Paper>
  );
};

export default PolarPlot;