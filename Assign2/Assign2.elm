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
import Html.Events exposing (onClick)



styles = Html.Attributes.style [("text-align", "center"),
                    ("font-weight", "bold"),
                    ("font-family", "\"Verdana\", Verdana, sans-serif"),
                    ("font-size", "35px"),
                    ("background-image", "url(\"http://clipground.com/images/athletic-field-clipart-16.jpg\")"),
                    ("background-repeat", "no-repeat"),
                    ("background-position","center bottom"),
                    ("background-size", "1300px 550px"), ("background-color","black")]

headerz = Html.Attributes.style [("font-size", "100px"),("color", "white")]


type alias Model =
    {
        pic1pos : (Float,Float),
        pic2pos : (Float,Float),
        mainscreen : Bool,
        one : Bool,
        onePlay: Bool,
        win1 : Bool,
        win2 : Bool,
        two: Bool,
        twoPlay: Bool,
        three : Bool,
        threePlay: Bool,
        win3  : Bool,
        lose : Bool
    }


type Msg
    = Tick Float
    | KeyMsg Key.KeyCode
    | Round1 
    | Round2
    | Round3


init : ( Model, Cmd Msg )
init =
    ({
        pic1pos = (0,200),
        pic2pos = (0,500),
        mainscreen = True,
        one = False,
        onePlay=False,
        win1 = False,
        win2 = False,
        two = False,
        twoPlay =False,
        three = False,
        threePlay = False,
        win3  = False,
        lose = False
        },  Cmd.none)


view : Model -> Html.Html Msg
view model = 
    if model.mainscreen == True then
       div [Html.Attributes.style [("text-align","center"),("background-image", "url(\"Clouds.jpg\")"),("height", "100%")]] [
        header [ Html.Attributes.style []] [
        img [src"title.png", Html.Attributes.style [("text-align", "center")]] []
                  ],
        audio [Html.Attributes.src "./simpsons.mp3", Html.Attributes.autoplay True] [],

        section [] [
        h2 [] [],
        img [src"homer.png", Html.Attributes.style [("text-align", "middle"),("width", "250px"),("height" ,"300px")]] [],
        img [src"bart.png", Html.Attributes.style [("text-align", "right"),("width", "150x"),("height", "300px")]] []
              
                   ],

        section [] [
        h2 [] [Html.text "Instruction"],
        p [Html.Attributes.style[("font-family", "\"Comic Sans ms\",cursive,sans serif")  ]] [Html.text "Marge promises to make food for whoever wins these races. Help Homer outrun Maggie,Bart and Lisa in 3 increasingly harder races by tapping the left and right arrow keys alternatively"],
        button [Html.Events.onClick Round1] [Html.text "Play"]
                    ]  
        ]

    else if (model.one == True) || (model.onePlay == True) then

        div [styles]
            [   header [Html.Attributes.style[("color", "white")]] [Html.text "Click on the arrow keys to start the race"],
        svg [    Svg.Attributes.width "1250",  Svg.Attributes.height "540"] [
            image [x (toString(first(model.pic1pos))) ,y (toString(second(model.pic1pos))) , Svg.Attributes.width "200", Svg.Attributes.height "200", xlinkHref "./hrunning.png"] [] ,
            image [x (toString(first(model.pic2pos))) ,y (toString(second(model.pic2pos))) , Svg.Attributes.width "100", Svg.Attributes.height "100", xlinkHref "./maggie.png"] [] ]]
        
    else if (model.two == True) || (model.twoPlay == True) then

        div [styles]
            [   header [Html.Attributes.style[("color", "white")]] [Html.text "Click on the arrow keys to start the race"],
        svg [    Svg.Attributes.width "1250",  Svg.Attributes.height "540"] [
            image [x (toString(first(model.pic1pos))) ,y (toString(second(model.pic1pos))) , Svg.Attributes.width "200", Svg.Attributes.height "200", xlinkHref "./hrunning.png"] [] ,
            image [x (toString(first(model.pic2pos))) ,y (toString(second(model.pic2pos))) , Svg.Attributes.width "200", Svg.Attributes.height "200", xlinkHref "./lrunning.png"] [] ]]
        
   else if (model.three == True) || (model.threePlay == True) then

        div [styles]
            [   header [Html.Attributes.style[("color", "white")]] [Html.text "Click on the arrow keys to start the race"],
        svg [    Svg.Attributes.width "1250",  Svg.Attributes.height "540"] [
            image [x (toString(first(model.pic1pos))) ,y (toString(second(model.pic1pos))) , Svg.Attributes.width "200", Svg.Attributes.height "200", xlinkHref "./hrunning.png"] [] ,
            image [x (toString(first(model.pic2pos))) ,y (toString(second(model.pic2pos))) , Svg.Attributes.width "250", Svg.Attributes.height "250", xlinkHref "./brunning.png"] [] ]]
          

    else if model.win1 == True then
        div [Html.Attributes.style [("text-align","center"),("background-image", "url(\"http://www.stickpng.com/assets/images/5a0c40a65a997e1c2cea116c.png\")"),("background-color","lightblue"),("height", "100%"),("background-repeat", "no-repeat")]] [
            header [ Html.Attributes.style []] [
            img [src"won.png", Html.Attributes.style [("text-align", "center")]] []
                  ],
            audio [Html.Attributes.src "./woho.wav", Html.Attributes.autoplay True] [],

            section [] [
            button [Html.Events.onClick Round2] [Html.text "Next Round"]
                    ]  ]
        

    else if model.win2 == True then
        div [Html.Attributes.style [("text-align","center"),("background-image", "url(\"http://www.stickpng.com/assets/images/5a0c40a65a997e1c2cea116c.png\")"),("background-color","lightblue"),("height", "100%"),("background-repeat", "no-repeat")]] [
            header [ Html.Attributes.style []] [
            img [src"won.png", Html.Attributes.style [("text-align", "center")]] []
                  ],
            audio [Html.Attributes.src "./woho.wav", Html.Attributes.autoplay True] [],

            section [] [
            button [Html.Events.onClick Round3] [Html.text "Next Round"]]]

    else if model.win3 == True then
        div [Html.Attributes.style [("text-align","center"),("background-image", "url(\"https://i1.wp.com/wallur.com/wp-content/uploads/2016/12/simpsons-background-1.jpg?fit=1920%2C1080\")"),("height", "100%"),("background-size", "1400px 700px"),("background-repeat", "no-repeat"),("background-position","top top")]] [
                    h1 [Html.Attributes.style [("font color","black"),("font-size", "60px")]] [Html.text "You've Won the Game "],
                    audio [Html.Attributes.src "./woho.wav", Html.Attributes.autoplay True] [],

                    Html.a [Html.Attributes.href "./App.html"]
                    [
                        button [] [Html.text "Play again!"]
                    ]  
                                        ]        

        
    else if model.lose == True then

        div [Html.Attributes.style [("text-align","center"),("background-image", "url(\"https://www.opencolleges.edu.au/informed/wp-content/uploads/2014/12/homer-doh.png\")"),("background-color","white"),("height", "100%"),("background-repeat", "no-repeat")]] [
        header [ Html.Attributes.style []] [
        img [src"lose.png", Html.Attributes.style [("text-align", "center")]] []
                  ],
        audio [Html.Attributes.src "./doh.wav", Html.Attributes.autoplay True] [],


        Html.a [Html.Attributes.href "./App.html"]
                    [
                        button [] [Html.text "Try again!"]
                    ] ]


    else div [styles]
            [   header [Html.Attributes.style[("color", "white")]] [Html.text "100m Sprint"]]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg {pic1pos,pic2pos,mainscreen,one,onePlay,win1 , win2 , two, twoPlay,three ,threePlay,win3 ,lose} = 
    
    if mainscreen == True then
        case msg of
            (Tick time) -> ({ 
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = True,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (KeyMsg _ ) -> ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = True,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)



            (Round1 ) -> ({
                    pic1pos = (50,200),
                    pic2pos = (50,300),
                    mainscreen = False,
                    one = True,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
            (Round2 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round3 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)        
        
        else if win1 == True then
        case msg of
            
            (Round2 ) -> ({
                    pic1pos = (0,200),
                    pic2pos = (0,300),
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = True,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)


            (Tick time) -> ({ 
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = win1,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (KeyMsg _ ) -> ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = True,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)

            (Round1 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round3 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)

        else if win2 == True then
        case msg of
            
            (Round3 ) -> ({
                    pic1pos = (0,200),
                    pic2pos = (0,300),
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = True,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)


            (Tick time) -> ({ 
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = win1,
                    win2 = win2,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = win3,
                    lose = False
                            },  Cmd.none)
                    
            (KeyMsg _ ) -> ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = True,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)

            (Round1 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round2 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)


    else if (first(pic1pos) > 1150) && (onePlay == True)   then
            ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = True,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
        
        else if (first(pic2pos) > 1150) && (onePlay == True)   then
            ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = True
                            },  Cmd.none)

        else if (first(pic1pos) > 1150) && (twoPlay == True)   then
            ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = True,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
        else if (first(pic2pos) > 1150) && (twoPlay == True)   then
            ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = True
                            },  Cmd.none)

        else if (first(pic1pos) > 1150) && (threePlay == True)   then
            ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = True,
                    lose = False
                            },  Cmd.none)
        else if (first(pic2pos) > 1150) && (threePlay == True)   then
            ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = True
                            },  Cmd.none)




    else if one == True then
        case msg of
            (Tick time) -> ({ 
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = True,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (KeyMsg _ ) -> ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=True,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (Round1 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round2 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round3 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)

    else if onePlay == True then
        case msg of
            (Tick time) -> ({ 
                    pic1pos = pic1pos,
                    pic2pos = (first(pic2pos)+toFloat(round <| abs(5* cos (time/1000))),second(pic2pos)),
                    mainscreen = False,
                    one = False,
                    onePlay= True,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (KeyMsg 37 ) -> ({
                    pic1pos = (first(pic1pos)+18, second(pic1pos)),
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=True,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
            (KeyMsg 39 ) -> ({
                    pic1pos = (first(pic1pos)+20, second(pic1pos)),
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=True,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
            (KeyMsg _ ) -> ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=True,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (Round1 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round2 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round3 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
    else if two == True then
        case msg of
            (Tick time) -> ({ 
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = True,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (KeyMsg _ ) -> ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =True,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (Round1 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round2 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round3 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)

    else if twoPlay == True then
        case msg of
            (Tick time) -> ({ 
                    pic1pos = pic1pos,
                    pic2pos = (first(pic2pos)+toFloat(round <| abs(6* cos (time/1000))),second(pic2pos)),
                    mainscreen = False,
                    one = False,
                    onePlay= False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =True,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (KeyMsg 37 ) -> ({
                    pic1pos = (first(pic1pos)+20, second(pic1pos)),
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =True,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
            (KeyMsg 39 ) -> ({
                    pic1pos = (first(pic1pos)+20, second(pic1pos)),
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =True,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
            (KeyMsg _ ) -> ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =True,
                    three = False,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (Round1 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round2 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round3 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)

    else if three == True then
        case msg of
            (Tick time) -> ({ 
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = True,
                    threePlay = False,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (KeyMsg _ ) -> ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = True,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (Round1 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round2 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round3 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)

   else if threePlay == True then
        case msg of
            (Tick time) -> ({ 
                    pic1pos = pic1pos,
                    pic2pos = (first(pic2pos)+toFloat(round <| abs(7* cos (time/1000))),second(pic2pos)),
                    mainscreen = False,
                    one = False,
                    onePlay= False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = threePlay,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (KeyMsg 37 ) -> ({
                    pic1pos = (first(pic1pos)+20, second(pic1pos)),
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = True,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
            (KeyMsg 39 ) -> ({
                    pic1pos = (first(pic1pos)+20, second(pic1pos)),
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = True,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
            (KeyMsg _ ) -> ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = False,
                    win2 = False,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = True,
                    win3  = False,
                    lose = False
                            },  Cmd.none)
                    
            (Round1) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round2 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)
            (Round3 ) -> ({pic1pos = pic1pos,pic2pos = pic2pos,mainscreen = mainscreen,one = one,onePlay= onePlay,win1 = win1,win2 = win2,two = two,twoPlay = twoPlay,three = three,threePlay = threePlay,win3  = win3,lose = lose
                            },  Cmd.none)

    else 
            ({
                    pic1pos = pic1pos,
                    pic2pos = pic2pos,
                    mainscreen = False,
                    one = False,
                    onePlay=False,
                    win1 = win1,
                    win2 = win2,
                    two = False,
                    twoPlay =False,
                    three = False,
                    threePlay = False,
                    win3  = win3,
                    lose = lose
                                                },  Cmd.none)







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

