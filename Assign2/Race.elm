module KeyboardExample exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Tuple exposing (first,second)
import Keyboard as Key


styles = Html.Attributes.style [("text-align", "center"),
                    ("font-weight", "bold"),
                    ("font-family", "\"Verdana\", Verdana, sans-serif"),
                    ("font-size", "35px"),
                    ("background-image", "url(\"http://clipground.com/images/athletic-field-clipart-16.jpg\")"),
                    ("background-repeat", "no-repeat"),
                    ("background-position","left bottom"),
                    ("background-size", "1700px 600px"), ("background-color","black")]

headerz = Html.Attributes.style [("font-size", "100px"),("color", "white")]

--https://images-na.ssl-images-amazon.com/images/I/71PJ6852%2BTL._SL1500_.jpg
--http://clipground.com/images/athletic-field-clipart-16.jpg
--https://t3.ftcdn.net/jpg/01/33/26/82/240_F_133268233_pxDNQxpzY2u59a2tPLtSsbnGq31mc9KH.jpg
type alias Model =
    {
        circle1Position : (Float,Float),
        circle2Position : (Float,Float),
        start : Bool,
        win : Bool,
        lose : Bool
    }


type Msg
    = Tick Float
    | KeyMsg Key.KeyCode


init : ( Model, Cmd Msg )
init =
    ({
        circle1Position = (50,575),
        circle2Position = (50,475),
        start = False,
        win = False,
        lose = False

        },  Cmd.none)


view : Model -> Html.Html Msg
view model = 
    if model.win == True then
                div [Html.Attributes.style [("height", "100%"), ("width", "100%"),("text-align", "center"), ("color", "white"),("background-image", "url(\"https://t3.ftcdn.net/jpg/01/33/26/82/240_F_133268233_pxDNQxpzY2u59a2tPLtSsbnGq31mc9KH.jpg\")"),("background-repeat", "no-repeat"),("background-position","center center"),("background-color","black")]
                ] 
                [
                
                    h1 [headerz] [Html.text "Congratulations You Win!"],
                    image [src "SonicStand.png"] [],
                    Html.a []
                    [
                        button [Html.Attributes.style [("font-size","30px")]] [Html.text "Play again!"]
                    ]]
        
    else

        div [styles]
            [   header [Html.Attributes.style[("color", "white")]] [Html.text "100m Sprint"],
        svg [    Svg.Attributes.width "2000",  Svg.Attributes.height "600"] [
            image [x (toString(first(model.circle1Position))) ,y (toString(second(model.circle1Position)-100)) , Svg.Attributes.width "100", Svg.Attributes.height "100" , xlinkHref "https://vignette.wikia.nocookie.net/spritechronicles/images/3/36/Wiki_sonic_icon.jpg/revision/latest?cb=20160918221002"] [] ,
            circle [cx (toString(first(model.circle2Position))) ,cy (toString(second(model.circle2Position))) , r "35", fill "white"] []]]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg {circle1Position, circle2Position, start, win, lose} =    if start == False then
            case msg of
         (Tick time) -> ({
                    circle1Position = circle1Position,
                    circle2Position = circle2Position,
                    start = False,
                    win = False,
                    lose = False
                             }, Cmd.none)
         (KeyMsg _) -> ({
                    circle1Position = circle1Position,
                    circle2Position = circle2Position,
                    start = True,
                    win = False,
                    lose = False
                             }, Cmd.none)

    else if first(circle1Position) > 1500 && first(circle2Position) < 1500 then
         ({
                    circle1Position = circle1Position,
                    circle2Position = circle2Position,
                    start = True,
                    win = True,
                    lose = False
                             }, Cmd.none)

    else if first(circle1Position) <1500 && first(circle2Position) > 1500 then
         ({
                    circle1Position = circle1Position,
                    circle2Position = circle2Position,
                    start = False,
                    win = False,
                    lose = True
                             }, Cmd.none)

    else 
        case msg of
            (KeyMsg _) -> ({
                    circle1Position = (first(circle1Position)+25, second(circle1Position)),
                    circle2Position = circle2Position,
                    start = True,
                    win = False,
                    lose = False
                             }, Cmd.none)

            (Tick time) -> ({
                    circle1Position = circle1Position,
                    circle2Position = (first(circle2Position)+toFloat(round <| abs(5* cos (time/1000))),second(circle2Position)),
                    start = True,
                    win = False,
                    lose = False
                            }, Cmd.none)





subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [
        Key.downs KeyMsg,
        Anim.times Tick
    ]


main : Program Never Model Msg
main = Html.program
          { init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
            }

