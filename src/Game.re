Random.self_init();

type celltype =
  | Truffle
  | Count(int)
  | Empty;

type boardcell = {
  celltype,
  mutable revealed: bool,
  mutable flagged: bool,
  index: int,
};

type row = array(boardcell);
type cells = array(row);

type board = {
  truffles: int,
  width: int,
  height: int,
  mutable revealed: int,
  triggered: bool,
  cells,
};

type config = {
  width: int,
  height: int,
  truffles: int,
};

type action =
  | StartGame
  | DigCell(boardcell)
  | Flag(boardcell);

type state =
  | Pregame(config)
  | Playing(board)
  | GameOver(bool, board);

module Styles = {
  open Css;
  let cell =
    style([minWidth(px(40)), minHeight(px(40)), padding(px(0))]);
};

let component = ReasonReact.reducerComponent("Game");

let default_config = {width: 8, height: 8, truffles: 10};

module IntSet =
  Set.Make({
    type t = int;
    let compare = compare;
  });

let truffle_gen = (num, max) => {
  let set = ref(IntSet.empty);
  while (IntSet.cardinal(set^) < num) {
    let new_truffle = Random.int(max);
    set := IntSet.add(new_truffle, set^);
  };
  set^;
};

let get_coords = (index, width) => (index / width, index mod width);

let neighbours = (index, width, height) => {
  let (y, x) = get_coords(index, width);
  let (left, right) = (x >= 1, x < width - 1);
  let (top, bottom) = (y >= 1, y < height - 1);
  let (above, below) = (index - width, index + width);
  List.concat([
    top
      ? (left ? [above - 1] : []) @ [above] @ (right ? [above + 1] : [])
      : [],
    (left ? [index - 1] : []) @ (right ? [index + 1] : []),
    bottom
      ? (left ? [below - 1] : []) @ [below] @ (right ? [below + 1] : [])
      : [],
  ]);
};

let neighbour_truffles = (truffles, index, width, height) =>
  IntSet.of_list(neighbours(index, width, height))
  |> IntSet.inter(truffles)
  |> IntSet.cardinal;

let new_cell = (index, truffles, width, height) =>
  if (IntSet.mem(index, truffles)) {
    {celltype: Truffle, revealed: false, flagged: false, index};
  } else {
    let count = neighbour_truffles(truffles, index, width, height);
    let celltype = count > 0 ? Count(count) : Empty;
    {celltype, revealed: false, flagged: false, index};
  };

let new_board = ({width, height, truffles}) => {
  let truffles_idxs = truffle_gen(truffles, width * height);
  {
    truffles,
    width,
    height,
    revealed: 0,
    triggered: false,
    cells:
      Array.init(height, y =>
        Array.init(width, x =>
          new_cell(y * width + x, truffles_idxs, width, height)
        )
      ),
  };
};

let (str, react_array) = (ReasonReact.string, ReasonReact.array);

module Cell = {
  let component = ReasonReact.statelessComponent("Cell");
  let make = (~cell, ~onClick, ~onContextMenu, ~playing, _children) => {
    ...component,
    render: _self =>
      switch (cell, playing) {
      | ({celltype: Truffle, flagged}, false) =>
        <button className=Styles.cell>
          {str(flagged ? {js|🚩|js} : {js|🥔|js})}
        </button>
      | ({revealed: false, flagged}, _) =>
        <button
          onClick={_evt => flagged ? () : onClick()}
          onContextMenu
          disabled={!playing}
          className=Styles.cell>
          {str({flagged ? {js|🚩|js} : {js|🍀|js}})}
        </button>
      | ({celltype, flagged}, _) =>
        <button
          className=Styles.cell
          disabled={celltype == Empty}
          onClick={_evt => flagged ? () : onClick()}>
          {switch (celltype) {
           | Truffle => str({js|🥔|js})
           | Count(i) => str(string_of_int(i))
           | Empty => <br />
           }}
        </button>
      },
  };
};

module Board = {
  let component = ReasonReact.statelessComponent("Board");
  let make = (~board, ~send=?, ~playing, _children) => {
    ...component,
    render: _self =>
      board.cells
      |> Array.mapi((r, row) =>
           <div key={"row" ++ string_of_int(r)}>
             {Array.map(
                cell =>
                  <Cell
                    cell
                    playing
                    key={string_of_int(cell.index)}
                    onClick={
                      switch (send) {
                      | None => (_ => ())
                      | Some(send) => (_ev => send(DigCell(cell)))
                      }
                    }
                    onContextMenu={
                      switch (send) {
                      | None => (_ => ())
                      | Some(send) => (
                          event => {
                            ReactEvent.Mouse.preventDefault(event);
                            send(Flag(cell));
                          }
                        )
                      }
                    }
                  />,
                row,
              )
              |> react_array}
           </div>
         )
      |> react_array,
  };
};

let reveal = (board: board, index) => {
  let (y, x) = get_coords(index, board.width);
  board.cells[y][x].revealed = true;
  board.revealed = board.revealed + 1;
  board;
};

let idx_to_cell = ({width, cells}, index) => {
  let (y, x) = get_coords(index, width);
  cells[y][x];
};

let flood = ({width, height} as board: board, index) => {
  open IntSet; // {add, is_empty, remove}
  let seen = ref(IntSet.empty);
  let tovisit = ref(IntSet.empty |> add(index));
  while (!is_empty(tovisit^)) {
    let index = IntSet.min_elt(tovisit^);
    seen := add(index, seen^);
    tovisit := remove(index, tovisit^);
    let cell = idx_to_cell(board, index);
    if (!cell.revealed) {
      cell.revealed = true;
      board.revealed = board.revealed + 1;
    };
    if (cell.celltype == Empty) {
      neighbours(index, width, height)
      |> List.iter(i =>
           if (!IntSet.mem(i, seen^)) {
             tovisit := add(i, tovisit^);
           }
         );
    };
  };
  board;
};

let next_game_state =
    ({revealed, width, height, truffles, triggered} as board) => {
  triggered
    ? GameOver(false, board)
    : {
      let win = revealed == width * height - truffles;
      // Js.log2(revealed, width * height - truffles);
      win ? GameOver(true, board) : Playing(board);
    };
};

let reveal_cell = (board, {index, revealed} as cell) =>
  if (!revealed) {
    switch (cell) {
    | {celltype: Truffle} => {...board, triggered: true}
    | {celltype: Count(_i)} => reveal(board, index)
    | {celltype: Empty} => flood(board, index)
    };
  } else {
    board;
  };

let reveal_neighbours = ({celltype, index}, {width, height, cells} as board) => {
  switch (celltype) {
  | Count(count) =>
    let neighbours =
      neighbours(index, width, height) |> List.map(idx_to_cell(board));
    let flagged = neighbours |> List.filter(cell => cell.flagged);
    if (count == List.length(flagged)) {
      neighbours
      |> List.fold_left(
           (board, {flagged} as cell) =>
             switch (flagged) {
             | false => reveal_cell(board, cell)
             | true => board
             },
           board,
         );
    } else {
      board;
    };
  | Empty
  | Truffle => board
  };
};

let make = _children => {
  ...component,

  initialState: () => {
    Playing(new_board(default_config));
  },

  reducer: (action, state) =>
    switch (action, state) {
    | (StartGame, Pregame(config)) =>
      ReasonReact.Update(Playing(new_board(config)))
    | (Flag(cell), _) =>
      cell.flagged = !cell.flagged;
      ReasonReact.Update(state);
    | (DigCell({revealed: false} as cell), Playing(board)) =>
      ReasonReact.Update(reveal_cell(board, cell) |> next_game_state)
    | (DigCell({revealed: true} as cell), Playing(board)) =>
      ReasonReact.Update(reveal_neighbours(cell, board) |> next_game_state)
    | _ => ReasonReact.NoUpdate
    },

  render: ({state, send}) => {
    switch (state) {
    | Pregame(config) =>
      <div>
        <p> {str("Config: " ++ string_of_int(config.width))} </p>
        <button onClick={_ev => send(StartGame)}> {str("Go!")} </button>
      </div>
    | Playing(board) => <Board board send playing=true />
    | GameOver(win, board) =>
      <>
        <Board board playing=false />
        <h1> {str(win ? {js|🎉 Congrats! 🎉|js} : "Boooo!")} </h1>
      </>
    };
  },
};