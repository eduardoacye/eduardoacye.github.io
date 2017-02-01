const alive = 1;
const dead = 0;

function make(nrows, ncols, makeCell) {
  return grid.make(nrows, ncols, makeCell);
}

var countRows = grid.countRows;
var countCols = grid.countCols;

function ref(life, i, j) {
  return grid.ref(life, i, j);
}

function set(life, i, j, cell) {
  grid.set(life, i, j, cell);
}

function neighbors(life, i, j) {
  return ref(life, i-1, j-1) + ref(life, i  , j-1) + ref(life, i+1, j-1) +
         ref(life, i-1, j  ) +          0          + ref(life, i+1, j  ) +
         ref(life, i-1, j+1) + ref(life, i  , j+1) + ref(life, i+1, j+1);
}

function cellFuture(life, i, j) {
  switch (neighbors(life, i, j)) {
  case 3:
    return alive;
  case 2:
    return ref(life, i, j);
  default:
    return dead;
  }
}

function evolve(life) {
  return make(countRows(life), countCols(life),
              (i, j) => cellFuture(life, i, j));
}

var life = {};
life.alive = alive;
life.dead = dead;
life.make = make;
life.countRows = countRows;
life.countCols = countCols;
life.ref = ref;
life.set = set;
life.evolve = evolve;
