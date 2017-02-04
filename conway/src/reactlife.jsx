class ConwaysGame extends React.Component {
  constructor(props) {
    super(props);
    const nrows = props.nrows;
    const ncols = props.ncols;
    this.state = {
      grid: life.make(nrows, ncols, () => life.dead),
      playing: false
    };
    this.handleGridClick = this.handleGridClick.bind(this);
    this.handlePlayPauseClick = this.handlePlayPauseClick.bind(this);
    this.handleClearClick = this.handleClearClick.bind(this);
    this.handleRandomizeClick = this.handleRandomizeClick.bind(this);
    this.evolve = this.evolve.bind(this);
  }
  componentDidMount() {
    this.gameId = setInterval(() => this.evolve(), 10);
  }
  
  componentWillUnmount() {
    clearInterval(this.gameId);
  }
  evolve() {
    if (this.state.playing) {
      this.setState((previous, props) => ({
        grid: life.evolve(previous.grid),
        playing: previous.playing
      }));
    }
  }
  handleGridClick(i, j) {
    const grid = this.state.grid;
    const cell = life.ref(grid, i, j);
    const modcell = (cell == life.alive) ? life.dead : life.alive;
    let modgrid = life.make(life.countRows(grid), life.countCols(grid),
                            (k, p) => ((i == k) && (j == p) ? modcell : life.ref(grid, k, p)));
    this.setState((previous, props) => ({
      grid: modgrid,
      playing: previous.playing
    }));
  }
  handlePlayPauseClick() {
    this.setState({
      grid: this.state.grid,
      playing: !this.state.playing
    });
  }
  handleClearClick() {
    const grid = this.state.grid;
    let modgrid = life.make(life.countRows(grid), life.countCols(grid), () => life.dead);
    this.setState({
      grid: modgrid,
      playing: this.state.playing
    });
  }
  handleRandomizeClick() {
    const grid = this.state.grid;
    let randomgrid = life.make(life.countRows(grid), life.countCols(grid),
                               () => this.randomCell());
    this.setState({
      grid: randomgrid,
      playing: this.state.playing
    });
  }
  
  randomCell() {
    const outcomes = [life.dead, life.alive];
    return outcomes[Math.floor(outcomes.length * Math.random())];
  }
  render() { 
    return (
        <div className="gol-main">
          <div className="gol-controls">
            <PlayPauseButton
              callback={this.handlePlayPauseClick}
              status={this.state.playing} />
            <ClearButton
              callback={this.handleClearClick} />
            <RandomizeButton
              callback={this.handleRandomizeClick} />
          </div>
          <VisualGrid
            callback={this.handleGridClick}
            grid={this.state.grid} />
        </div>
    );
  }
}

function PlayPauseButton(props) {
  const status = props.status;
  if (status) {
    return (
        <button onClick={props.callback} className="btn-pause">
          <b>Pause</b>
        </button>
    );
  } else {
    return ( 
        <button onClick={props.callback} className="btn-play">
          <b>Play</b>
        </button>
    );
  }
}

function ClearButton(props) {
  return ( 
      <button onClick={props.callback} className="btn-clear">
        <b>Clear</b>
      </button>
  );
}

function RandomizeButton(props) {
  return (
      <button onClick={props.callback} className="btn-randomize">
        <b>Randomize</b>
      </button> 
  );
}

function VisualGrid(props) {
  const grid = props.grid;
  let rows = Array(life.countRows(grid));
  for (let i = 0; i < life.countRows(grid); i++) {
    let arr = Array(life.countCols(grid));
    for (let j = 0; j < life.countCols(grid); j++) {
      let cell = life.ref(grid, i, j);
      arr[j] = (
          <td key={j} className="cell" style={{backgroundColor: (cell == life.alive ? "white" : "black")}}
              onClick={ () => props.callback(i, j) }
            />
      );
    }
    rows[i] = (
        <tr key={i}>
          { arr }
        </tr>
    );
  }
  return (
      <table className="grid">
        <tbody>
          { rows }
        </tbody>
      </table>
  );
}

ReactDOM.render(
  <ConwaysGame ncols={45} nrows={45} />,
  document.getElementById('root')
);
