port module Main exposing (..)

import Browser

import Html exposing (Html, div, text, button, input, form, ul, li)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (value, type_)

-- MODEL
type alias Task = { description: String }
type alias ApplicationState = { tasks: List Task, draft: Task }

empty_task_list : List Task
empty_task_list = []

empty_task : Task
empty_task = Task ""

state : ApplicationState
state = ApplicationState empty_task_list empty_task

-- UPDATE
type TaskFormAction = ChangeDescription String | Submit
type Action = RemoveTask Task | FormAction TaskFormAction

-- PORTS
port setStorage : (List Task) -> Cmd message

-- Parse, don't validate (https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
validate_task : Task -> Result () Task
validate_task task = 
  if not (List.any (\validation -> validation == False) [String.length(task.description) > 0])
    then Ok task
    else Err ()

update : Action -> ApplicationState -> (ApplicationState, Cmd message)
update action appState = case action of
    RemoveTask task -> 
      let stateUpdated = { appState | tasks = List.filter (\fromlist -> task /= fromlist) appState.tasks } in (stateUpdated, setStorage stateUpdated.tasks)
            
    FormAction (ChangeDescription description) ->
      let stateUpdated = { appState | draft = { description = description }} in (stateUpdated, Cmd.none)
        
    FormAction Submit -> 
      case validate_task(appState.draft) of
        Ok task -> let stateUpdated = { appState | tasks = appState.tasks ++ [task], draft = empty_task } in (stateUpdated, setStorage stateUpdated.tasks)
        Err _ -> (appState, Cmd.none)

-- VIEW
render_task : Task -> Html Action
render_task task = 
  button [onClick (RemoveTask task)] [ text task.description ]

view : ApplicationState -> Html Action
view appState =
  div [] [
    ul [] (List.map (\task -> li [] [render_task task]) appState.tasks),
    form [onSubmit (FormAction Submit)] [
      input [
        type_ "text",
        onInput (\value -> FormAction (ChangeDescription value)),
        value appState.draft.description
      ] [],
      button [onClick (FormAction Submit)] [ text "Add Task" ]
    ]
  ]


-- FLAGS
init : List Task -> (ApplicationState, Cmd message)
init savedState = (ApplicationState savedState empty_task, Cmd.none)

-- MAIN
main : Program (List Task) (ApplicationState) Action
main = Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
