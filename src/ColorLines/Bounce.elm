module ColorLines.Bounce exposing (bounce)

bounce frequency friction =
  let
    fx = max 1 (frequency / 20)
    fr = 20 ^ (friction / 100)
    a t =
      ((fr / 10) ^ (-t)) * (1 - t)

    fn t =
      let
        b = pi / 2
        angle = fx * t + b
      in
        (a t) * (cos angle)
  in
    fn
