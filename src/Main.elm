
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit, onInput)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    { todos: List Todo
    , inputText : String
    }

type alias Todo = 
    { text : String 
    , completed : Bool
    }

init : Model
init = 
    Model [] ""

type Message
    = AddTodo
    | RemoveTodo Int
    | ToggleTodo Int
    | ChangeInput String

update : Message -> Model -> Model
update message model
    = case message of 
        AddTodo ->
            { model 
            | todos = addToList model.inputText model.todos
            , inputText = ""
            }
        RemoveTodo index ->
            { model | todos = removeFromList index model.todos}
        ToggleTodo index ->
            {model | todos = toggleAtIndex index model.todos}
        ChangeInput input ->
            {model | inputText = input}


addToList : String -> List Todo -> List Todo
addToList input todos =
    todos ++ [{ text = input, completed = False}]

removeFromList : Int -> List Todo -> List Todo
removeFromList index list =
    List.take index list ++ List.drop (index + 1) list

toggleAtIndex : Int -> List Todo -> List Todo
toggleAtIndex  indexToToggle list =
    List.indexedMap (\currentIndex todo ->
        if currentIndex == indexToToggle then
            { todo | completed = not todo.completed}
        else 
            todo
    ) list

changeInput : String -> Model -> Model
changeInput string model =
    { model | inputText = string}

viewTodo : Int -> Todo -> Html Message
viewTodo index todo = 
    li
        [style "text-decoraction"
            (if todo.completed then
                "line-through"
             else
                "none"
            )
        ]
        [ text todo.text
        -- , button [ type_ "button", onClick (ToggleTodo index) ] [text "Toggle"]
        , button [ type_ "button", onClick (RemoveTodo index) ] [ text "Delete" ]
        -- , button [ type_ "button", onClick (CompleteTodo index) ] [ text "Complete" ]
        ]


view : Model -> Html Message
view model = 
    Html.form [ onSubmit AddTodo] 
        [ h1 [ class "title"] [ text "Todos in Elm"]
        , input [value model.inputText, onInput ChangeInput, placeholder "What you want to do?"] []
        , if List.isEmpty model.todos then
            p [] [text "The list is cleeean" ]
          else 
            ol [] (List.indexedMap viewTodo model.todos )
        ]














-- type alias Model = 
--     { title: String
--      ,description: String
--      ,tasks: Record
--     }

-- init: Model
-- init = 
--     Model "" "" []

-- type Msg 
--     = Title String
--     | Description String
--     | Save

-- update : Msg -> Model -> Model
-- update msg model =
--   case msg of
--     Title title ->
--         {model | title = title}
    
--     Description description ->
--         {model | description = description}
    
--     Save title description -> 
--         {model | title :: tasks}

-- view model =
--   div []
--       [ input [placeholder "Enter title", value model.title, onInput Title][]
--       , input [placeholder "Enter description", value model.description, onInput Description][]
--       , button [onClicK Save][text "Save"]
--       , div [] [text ("Title: " ++ model.title)]
--       , div [] [text ("Description: " ++ model.description)]
--       ]

 

--        -- button [ onClick Decrement ] [ text "-" ]
--     -- , div [] [ text (String.fromInt model) ]
--     -- , button [ onClick Increment ] [ text "+" ]
