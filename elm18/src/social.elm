module Social exposing (..)

import Html exposing (text, img, button, div, textarea, Html, pre,br, input, span,a,table,td,tr)
import Html.Attributes exposing (src, type_, value, placeholder, checked, style,href,maxlength,size,target,wrap,rows,cols)
import Html.Events exposing (onClick,onInput)
import WebSocket
--import File.Select as Select
--import File exposing (File)
import Json.Decode as D
import Json.Encode as E
import List
import String
import Debug
--import Dialog
import Result

main =
    Html.program
        { init = init, view = view, update = update, subscriptions = subscriptions
        }


echoServer : String
echoServer = "ws://localhost:8080/websocket"

-- MODEL
type alias  LoginType =  { username: String, password : String, is_login : Bool }
type alias IMType = {sender: String, message: String}
type alias Model = 
    { messages: List String       -- messages from server for debugging
    , err: String                 -- Error message to be displayed
    , login : LoginType
    , list_client : List String   -- list of usernames logged in
    , list_im : List IMType       -- list of instant messages
    , new_im : String             -- new instant message to send
    }

default_model : Model
default_model =
    {messages=[], err = "", list_client=[],list_im=[],new_im="", login={username="",password="",is_login=False}}
                
init : (Model, Cmd Msg)
init =
    (default_model,  Cmd.none)
        
-- UPDATE

type Msg
  = SocketMessage String -- websocket messages
  | MsgUsername String
  | MsgPassword String
  | TryLogin
  | TryLogout
  | TryCreateAccount
  | TryResetPassword
  | MsgNewIM String
  | TrySendIM

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        log=model.login
    in
        case msg of
            SocketMessage str -> -- handle all websocket messages
                (handlesocket model str, Cmd.none)
            MsgPassword str ->
                ({model | login= {log | password=str}}, Cmd.none)
            MsgUsername str ->
                ({model | login= {log | username=str}}, Cmd.none)
            TryLogin ->
                (model, WebSocket.send echoServer ("{\"login\":{\"username\":\"" ++ log.username ++ "\",\"password\":\"" ++ log.password ++ "\"}}"))
            TryCreateAccount ->
                (model, WebSocket.send echoServer ("{\"create_account\":{\"username\":\"" ++ log.username ++ "\",\"password\":\"" ++ log.password ++ "\"}}"))
            TryResetPassword ->
                (model, WebSocket.send echoServer ("{\"reset_password\":{\"username\":\"" ++ log.username ++ "\"}}"))
            TryLogout ->
                (default_model, WebSocket.send echoServer "{\"logout\":[]}")
            MsgNewIM str ->
                ({model | new_im = str}, Cmd.none)
            TrySendIM ->
                ({model | new_im=""}, WebSocket.send echoServer ("{\"imessage\":\"" ++ model.new_im ++ "\"}"))

handlesocket : Model -> String -> Model
handlesocket model1 str =
    let
        action = D.decodeString (D.field "action" D.string) str
        data = D.decodeString (D.field "data" D.value) str
        model = case str of
                    "ping" ->
                        model1
                    _ ->
                        {model1 | messages=(str :: model1.messages)}
    in
        case action of
            Ok s ->
                case data of
                    Ok v ->
                        (handle_action model s v)
                    _ ->
                        model
            _ ->
                model

handle_action : Model -> String -> D.Value -> Model
handle_action model str val =
    case str of
        "login" ->
            (login model val)
        "create_account" ->
            (login model val)
        "reset_password" ->
            (reset_password model val)        
        "list_client" ->
            {model | list_client =(Result.withDefault [] (D.decodeValue( D.list D.string) val)) }
        "new_im" ->
            {model | list_im = (List.reverse
                                    ({sender = (value2str "sender" val),
                                          message = (value2str "message" val)}
                                    :: model.list_im))}
        _ ->
            model

value2str : String -> D.Value -> String
value2str str val =
    Result.withDefault "" (D.decodeValue (D.field str D.string) val)

reset_password : Model -> D.Value -> Model
reset_password model val =
    let
        msg = Result.withDefault "" (D.decodeValue D.string val)
        login = model.login
    in
        {model | login={login | password = ""},err = msg}
        
        
login : Model -> D.Value -> Model
login model val =
    let
        res = Result.withDefault False (D.decodeValue (D.field "result" D.bool) val)
        merr = Result.withDefault "" (D.decodeValue (D.field "error" D.string) val)
        login = model.login
    in
        {model | login={login| is_login = res}, err = merr}
            
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen echoServer SocketMessage

-- VIEW


view : Model -> Html Msg
view model =
    div [] [maybe_show_login model.login
           ,case model.login.is_login of
                True ->
                    main_page model
                False ->
                    div [] []
           ,div [] [text model.err]
           ,div [] (List.map viewMessage (List.reverse model.messages))
           ]
main_page : Model -> Html Msg
main_page model =
    let
        myst = [style [("vertical-align","top")]]
    in
        table []
            [tr []
                 [ td myst [td [] (List.map (view_im model.login) model.list_im )
                           , enter_new_im model.new_im]
                 , td myst (List.map viewMessage model.list_client)
                 ]]

view_im : LoginType -> IMType -> Html Msg
view_im login im  =
               let
                   color = case im.sender == login.username of
                               True ->
                                   "red"
                               False ->
                                   "black"
               in
                   div [style [("color",color)]] [text (im.sender ++ " : " ++ im.message)]

enter_new_im : String -> Html Msg                       
enter_new_im new_im =
    div [] [text "Message "
           ,textarea [cols 50,rows 10, wrap "hard", value new_im, onInput MsgNewIM ] []
           , br [] []
           ,button [onClick TrySendIM] [text "Send Message"]
           ]
        
maybe_show_login : LoginType -> Html Msg
maybe_show_login login =
    case login.is_login of
        True ->
            div [] [ text ("Welcome " ++ login.username ++ " ")
                   , button [ onClick TryLogout ] [ text "Logout" ] ]
        False ->
            div []
                [ input [ type_ "text", placeholder "Username", onInput MsgUsername ] []
                , input [ type_ "password", placeholder "Password", onInput MsgPassword ] []
                ,br [] []
                , button [ onClick TryLogin ] [ text "Login" ]
                ,br [] []
                , button [ onClick TryCreateAccount ] [ text "Create Account" ]
                ,br [] []
                , button [ onClick TryResetPassword ] [ text "Reset Password" ]
                ]
            
viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]

