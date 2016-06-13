module Opi = Opium.Std

let print_param = Opi.get "/hello/:name" begin fun req ->
  `String ("Hello " ^ Opi.param req "name") |> Opi.respond'
end

let home = Opi.get "/" begin fun _ ->
  `String (Sinsi_view.render_home_page ()) |> Opi.respond'
end

let channels_route = Opi.get "/channels" begin fun _ ->
  `String (Sinsi_view.render_channels_page Sinsi_feeds.channel_store) |> Opi.respond'
end

let feeds_route = Opi.get "/feeds" begin fun _ ->
    `String (Sinsi_feeds.channel_store_to_json Sinsi_feeds.channel_store) |> Opi.respond'
end

let run_server () =
  let port = 4000 in
  Lwt.async(fun () -> Lwt_io.printlf "Running server at http://localhost:%d" port);
  Opi.App.empty
  |> print_param
  |> home
  |> feeds_route
  |> channels_route
  |> Opi.App.port port
  |> Opi.App.run_command

let main () =
  Lwt.return (run_server ())

let start_feed_worker () =
  let open Lwt.Infix in
  Sinsi_feeds.channel_store >>= fun store ->
  Sinsi_feeds.feed_update_worker store

let print_channels () =
  let open Lwt.Infix in
  Sinsi_feeds.channel_store >>= fun store ->
  Sinsi_feeds.print_channels_worker Sinsi_feeds.feeds store

let _ =
  Lwt.async (start_feed_worker);
  Lwt.async (print_channels);
  Lwt_main.run (main ())
