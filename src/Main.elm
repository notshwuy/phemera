module Main exposing (..)

import Browser

import Html exposing (Html, div, text)
import Html exposing (button)

import Html.Events exposing (onClick)
import Html exposing (input)
import Html exposing (Html, div, text, button, input, form, ul, li)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (value, type_)

-- MODEL
type alias Task = { description: String }
type alias ApplicationState = { tasks: List Task, carry: Task }

emptyTaskList : List Task
emptyTaskList = []

emptyTask : Task
emptyTask = Task ""

state : ApplicationState
state = ApplicationState emptyTaskList emptyTask

-- UPDATE
type TaskFormAction = ChangeDescription String | Submit
type Action = RemoveTask Task | FormAction TaskFormAction

-- Parse, don't validate (https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
validate_task : Task -> Result () Task
validate_task task = 
  if not (List.any (\validation -> validation == False) [String.length(task.description) > 0])
    then Ok task
    else Err ()

update : Action -> ApplicationState -> ApplicationState
update action appState =
  case action of
    RemoveTask task -> 
      { appState | tasks = List.filter (\fromlist -> task /= fromlist) appState.tasks }
            
    FormAction (ChangeDescription description) ->
      { appState | carry = { description = description }}
        
    FormAction Submit -> 
      case validate_task(appState.carry) of
        Ok task -> { appState | tasks = appState.tasks ++ [task], carry = emptyTask }
        Err _ -> appState

-- VIEW
renderTask : Task -> Html Action
renderTask task = 
  button [onClick (RemoveTask task)] [ text task.description ]

view : ApplicationState -> Html Action
view appState =
  div []
  [
    ul [] (List.map (\task -> li [] [renderTask task]) appState.tasks),
    form [onSubmit (FormAction Submit)] [
      input [
        type_ "text",
        onInput (\value -> FormAction (ChangeDescription value)),
        value appState.carry.description
      ] [],
      button [onClick (FormAction Submit)] [ text "Add Task" ]
    ]
  ]

-- MAIN
    
main : Program () (ApplicationState) Action
main = Browser.sandbox { init = state, update = update, view = view }
