[%%debugger.chrome];

[@bs.val] [@bs.scope "module"] external isHotEnabled: bool = "hot";

[@bs.val] [@bs.scope ("module", "hot")]
external hotAccept: unit => unit = "accept";

ReactDOMRe.renderToElementWithId(<Game />, "game");

if (isHotEnabled) {
  hotAccept();
};