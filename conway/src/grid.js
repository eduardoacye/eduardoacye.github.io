function make(nrows, ncols, makeValue) {
  let g = Array(nrows);
  for (let i = 0; i < nrows; i++) {
    g[i] = Array(ncols);
    for (let j = 0; j < ncols; j++) {
      g[i][j] = makeValue(i, j);
    }
  }
  return g;
}

function countRows(grid) {
  return grid.length;
}

function countCols(grid) {
  return grid[0].length;
}

function modulo(a, b) {
  return ((a % b) + b) % b;
}

function ref(grid, i, j) {
  return grid[modulo(i, countRows(grid))][modulo(j, countCols(grid))];
}

function set(grid, i, j, value) {
  grid[modulo(i, countRows(grid))][modulo(j, countCols(grid))] = value;
}

var grid = {};
grid.make = make;
grid.countRows = countRows;
grid.countCols = countCols;
grid.ref = ref;
grid.set = set;
