module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html exposing (button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)

main : Program () (List Task) Action
main = Browser.sandbox { init = tasks, update = update, view = view }

-- MODEL
type alias Task = { description: String, done: Bool }

tasks : List Task
tasks = [
  { description = "Learn Elm", done = False }, 
  { description = "Do networking stuff", done = False }]

-- UPDATE
type Action = AddTask Task | RemoveTask Task

update : Action -> List Task -> List Task
update action current_tasks =
  case action of
    AddTask task -> current_tasks ++ [task]
    RemoveTask task -> List.filter (\task_fromlist -> task /= task_fromlist) current_tasks

-- VIEW

renderTask : Task -> Html Action
renderTask task = 
  button [onClick (RemoveTask task), disabled task.done] [ text task.description ]
      
view : List Task -> Html Action
view current_tasks =
  div []
    (List.map (\x -> renderTask x) current_tasks)
    
