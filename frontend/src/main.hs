import Frontend
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom

import Common.Route

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder backendRouteEncoder
  run $ runFrontend validFullEncoder frontend
