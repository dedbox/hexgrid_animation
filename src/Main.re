open Bonsai_web;

module Model = {
  open Sexplib.Std;

  [@deriving sexp]
  type t = unit;

  let equal = (==);
  let default: t = ();
};

open Virtual_dom.Tyxml;

module TwoD = {
  module Point = {
    type t = (float, float);
    type ts = list(t);

    // o operator o, point on both
    let (+) = ((x1, y1), (x2, y2)) => (x1 +. x2, y1 +. y2);
    let ( * ) = ((x1, y1), (x2, y2)) => (x1 *. x2, y1 *. y2);

    // o>  operator o, point on right
    // o<  operator o, point on left
    let ( *> ) = (a, (x, y)) => (a *. x, a *. y);
    let ( *< ) = ((x, y), a) => (x *. a, y *. a);

    // o* operator o, points on right
    // o*< operator o, points on left
    let ( +* ) = (p: t, ps: ts): ts => ps |> List.map((+)(p));
    let (+*<) = (ps: ts, p: t): ts => ps |> List.map((+)(p));
  };

  module Hexagon = {
    type t = list(Point.t);

    let point = (size: float, i: int): Point.t => {
      let theta = Float.(of_int(i) *. pi /. 3.0);
      Point.(size *> (cos(theta), sin(theta)));
    };

    let points = (size: float): t => point(size) |> List.init(6);
  };
};

let e = <animation fill="freeze" values="1 2" />;

let drawing =
  <svg>
    <polygon
      points={TwoD.Hexagon.points(50.0)}
      fill="gold"
      stroke="black"
      stroke_width="4"
      transform=[`Translate((100.0, Some(100.0)))]>
      <animateTransform
        attributeName="transform"
        type_="scale"
        from="1"
        to_="1"
        values="1; 0; 1"
        keyTimes="0; 0.5; 1"
        dur="2s"
        repeatCount="indefinite"
        additive="sum"
      />
    </polygon>
  </svg>;

let _: Start.Handle.t(_) =
  Start.start_standalone(
    ~initial_input=(),
    ~bind_to_element_with_id="container",
    Bonsai.const(drawing |> Svg.toelt),
  );
