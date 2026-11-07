module Make (G : Game.S) = struct
  module B = Multi_br.Make (Multi.Of_game (G))

  let index = function Game.P0 -> 0 | P1 -> 1
  let value p sigma = B.value (index p) sigma
  let strategy p sigma = B.strategy (index p) sigma
  let nash_conv sigma = value P0 sigma +. value P1 sigma
  let exploitability sigma = nash_conv sigma /. 2.
end
