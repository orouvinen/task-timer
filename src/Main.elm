module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Task as T exposing (..)


type IconSize
    = Small
    | Normal



{-
   How often to receive update on current time.
   Determines the update interval of running task's time display.
-}


tickPeriod : Time
tickPeriod =
    1 * Time.second


type alias TaskId =
    Int


type alias ProjectId =
    Int


type alias TimeFragment =
    { taskId : TaskId
    , orderNum : Int
    , start : Time
    , end : Maybe Time
    }


type alias Task =
    { id : TaskId
    , name : String
    , running : Bool
    , editing : Bool
    , elapsed : Time
    , projectId : ProjectId
    }


type alias Project =
    { name : String
    , id : ProjectId
    , expanded : Bool
    }


type alias Model =
    { projects : List Project
    , tasks : List Task
    , projectNameInput : Maybe String
    , nextProjectId : Int
    , nextTaskId : Int
    , runningTask : Maybe Task
    , timeFragments : List TimeFragment
    }


type Msg
    = CreateProject String
    | DeleteProject ProjectId
    | ToggleProjectExpand ProjectId
    | ProjectNameInputStart
    | ProjectNameInputChange String
    | CancelCreateProject
    | AddTask ProjectId
    | DeleteTask TaskId
    | TaskNameInputStart ProjectId
    | TaskNameInputChange TaskId String
    | ToggleTaskEdit TaskId String
    | CancelCreateTask
    | SetTaskRunning Task Bool
    | CutTimeFragment (Maybe Task) (Maybe Task) Time
    | UpdateTaskTime (Maybe Task) Time
    | Tick Time


init : ( Model, Cmd Msg )
init =
    model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every tickPeriod Tick


main : Program Never Model Msg
main =
    program { init = init, view = view, update = update, subscriptions = subscriptions }


model : Model
model =
    { projects = []
    , tasks = []
    , projectNameInput = Nothing
    , nextProjectId = 1
    , nextTaskId = 1
    , runningTask = Nothing
    , timeFragments = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateProject projectName ->
            { model
                | projects =
                    model.projects
                        |> append { id = model.nextProjectId, name = projectName, expanded = False }
                , nextProjectId = model.nextProjectId + 1
                , projectNameInput = Nothing
            }
                ! []

        DeleteProject projectId ->
            model ! []

        ToggleProjectExpand id ->
            let
                toggleExpanded : Project -> Project
                toggleExpanded project =
                    { project
                        | expanded =
                            if project.id == id then
                                not project.expanded
                            else
                                project.expanded
                    }
            in
                { model | projects = List.map toggleExpanded model.projects } ! []

        ProjectNameInputStart ->
            { model | projectNameInput = Just "" } ! []

        ProjectNameInputChange name ->
            { model | projectNameInput = Just name } ! []

        AddTask projectId ->
            { model
                | tasks =
                    model.tasks
                        |> append { id = model.nextTaskId, projectId = projectId, name = "", running = False, editing = True, elapsed = 0 }
                , nextTaskId = model.nextTaskId + 1
            }
                ! []

        TaskNameInputChange id name ->
            let
                setTaskName newName task =
                    { task
                        | name =
                            if task.id == id then
                                newName
                            else
                                task.name
                    }
            in
                { model | tasks = List.map (setTaskName name) model.tasks } ! []

        {- The way textInput works, it will pass the final input value together with the end msg.
           We don't need the value because it has already been taken care of in TaskNameInputChange, so
           it's discarded here.
        -}
        ToggleTaskEdit id _ ->
            let
                toggleEdit task =
                    { task
                        | editing =
                            if task.id == id then
                                not task.editing
                            else
                                task.editing
                    }
            in
                { model | tasks = List.map toggleEdit model.tasks } ! []

        DeleteTask id ->
            { model | tasks = List.filter (\t -> t.id /= id) model.tasks } ! []

        CancelCreateTask ->
            model ! []

        SetTaskRunning targetTask runningState ->
            let
                prevTask =
                    model.runningTask

                nextTask =
                    if runningState == True then
                        Just targetTask
                    else
                        Nothing

                setRunning task =
                    { task
                        | running =
                            if task.id == targetTask.id then
                                runningState
                            else
                                {-
                                   Regardless of whether the selected task is set to running or not, all other
                                   tasks should be stopped anyway, i.e. only one task can be running at any
                                   given time. I'm just a man, you know.
                                -}
                                False
                    }
            in
                { model
                    | runningTask = nextTask
                    , tasks = List.map setRunning model.tasks
                }
                    ! [ T.perform (CutTimeFragment prevTask nextTask) Time.now ]

        CutTimeFragment prevTask nextTask time ->
            {-
               Two things need to be achieved here:
               1. If there's a task currently running, find and end its latest time fragment.
               2. If there's another task starting, then start new time fragment for it.
            -}
            let
                -- Create a new time fragment starting from now
                startingTimeFragment =
                    nextTask
                        |> Maybe.andThen
                            (\task -> Just { taskId = task.id, start = time, end = Nothing, orderNum = timeFragmentCount model task })

                -- Find the time fragment that was previously started
                endingTimeFragment =
                    prevTask
                        |> Maybe.andThen
                            (\task -> getLatestTimeFragment model task)

                -- Append startingTimeFragment to model
                newTimeFragments =
                    case startingTimeFragment of
                        Just fragment ->
                            model.timeFragments |> append fragment

                        Nothing ->
                            model.timeFragments

                updateFragmentEnd orderNum endTime fragment =
                    let
                        taskId =
                            Maybe.map .id prevTask |> Maybe.withDefault 0
                    in
                        { fragment
                            | end =
                                if fragment.orderNum == orderNum && taskId == fragment.taskId then
                                    Just endTime
                                else
                                    fragment.end
                        }
            in
                { model
                    | timeFragments =
                        case endingTimeFragment of
                            Just fragment ->
                                List.map (updateFragmentEnd fragment.orderNum time) newTimeFragments

                            Nothing ->
                                newTimeFragments
                }
                    ! []

        -- Received periodically
        Tick time ->
            update (UpdateTaskTime model.runningTask time) model

        UpdateTaskTime runningTask time ->
            case runningTask of
                Just t ->
                    let
                        updateTask task =
                            if task.id == t.id then
                                { task | elapsed = taskTotalTime model task time }
                            else
                                task
                    in
                        { model | tasks = List.map updateTask model.tasks } ! []

                Nothing ->
                    model ! []

        _ ->
            model ! []



view : Model -> Html Msg
view model =
    div [] <|
        [ div [ class "list-group" ] <|
            let
                projects =
                    projectList model

                showProjects =
                    if List.length projects > 0 then
                        projects
                    else
                        [ div [ class "text-muted" ] [ text "No projects" ] ]
            in
                showProjects
        ]
            ++ case model.projectNameInput of
                Just projectName ->
                    textInput projectName "Project name" ProjectNameInputChange CreateProject

                Nothing ->
                    [ div [ class "outer-controls" ]
                        [ button
                            [ onClick ProjectNameInputStart
                            , class "btn btn-primary btn-lg"
                            ]
                            [ text "New project" ]
                        ]
                    ]


projectList : Model -> List (Html Msg)
projectList model =
    List.map (project model) model.projects


projectHeader : Model -> Project -> Html Msg
projectHeader model project =
    let
        taskCount =
            List.length <| projectTasks model project
    in
        span []
            [ span [ class "text-muted" ] [ text "Project" ]
            , h2 [ onClick ProjectNameInputStart ]
                [ text <|
                    project.name
                        ++ " ("
                        ++ toString taskCount
                        ++ nounPlural " task" taskCount
                        ++ ")"
                ]
            ]


project : Model -> Project -> Html Msg
project model project =
    div [ class "list-group-item" ] <|
        [ projectHeader model project
        , taskCollapserButton project
        ]
            ++ if project.expanded then
                let
                    tasks =
                        projectTasks model project
                in
                    taskList model tasks
                        :: [ div [ class "project-controls" ]
                                [ button
                                    [ type_ "button"
                                    , class "btn btn-outline-success"
                                    , onClick <| AddTask project.id
                                    ]
                                    [ text "New task" ]
                                ]
                           ]
               else
                []


taskList : Model -> List Task -> Html Msg
taskList model tasks =
    if List.length tasks == 0 then
        div [ class "task-list text-muted" ] [ text "No tasks" ]
    else
        div [ class "task-list" ] <| List.map (task model) tasks


task : Model -> Task -> Html Msg
task model task =
    div []
        [ if task.editing then
            case getTask model task.id of
                Just task ->
                    span [] <| textInput task.name "Task name" (TaskNameInputChange task.id) (ToggleTaskEdit task.id)

                -- Shouldn't happen
                Nothing ->
                    text "Invalid task id"
          else
            div [ class "row" ]
                [ div
                    [ class "col-sm-4 task-list-item" ]
                    [ text task.name ]
                , div
                    [ class "sol-sm-2 task-timer" ]
                  <|
                    taskTimeDisplay task
                , div
                    [ class "col-sm-6" ]
                  <|
                    taskControls task
                ]
        ]


taskTimeDisplay : Task -> List (Html Msg)
taskTimeDisplay task =
    let
        timerDigit x =
            toString x
                |> (++)
                    (if x < 10 then
                        "0"
                     else
                        ""
                    )

        totalSeconds =
            round <| inSeconds task.elapsed

        seconds =
            rem totalSeconds 60

        minutes =
            rem (totalSeconds // 60) 60

        hours =
            totalSeconds // 3600
    in
        [ span
            [ class <|
                "task-time"
                    ++ (if task.running then
                            " running"
                        else
                            ""
                       )
            ]
            [ text <| (timerDigit hours) ++ ":" ++ (timerDigit minutes) ++ ":" ++ (timerDigit seconds) ]
        ]


taskControls : Task -> List (Html Msg)
taskControls task =
    [ if not task.running then
        button
            [ class "btn btn-outline-dark btn-sm", onClick <| SetTaskRunning task True ]
            [ icon Small "play_arrow" ]
      else
        button
            [ class "btn btn-outline-dark btn-sm", onClick <| SetTaskRunning task False ]
            [ icon Small "pause" ]
    , button
        [ class "btn btn-success btn-sm" ]
        [ icon Small "done" ]
    , button
        [ class "btn btn-danger btn-sm"
        , onClick <| DeleteTask task.id
        ]
        [ icon Small "delete" ]
    ]


taskCollapserButton : Project -> Html Msg
taskCollapserButton project =
    let
        action =
            if not project.expanded then
                "Show"
            else
                "Hide"

        buttonIcon =
            if not project.expanded then
                "arrow_drop_down"
            else
                "arrow_drop_up"
    in
        button
            [ class "btn btn-link"
            , onClick <| ToggleProjectExpand project.id
            ]
            [ text <| action ++ " tasks", icon Small buttonIcon ]



{- textInput creates a text input element together with a button that accepts the given input.
   Args:
       itemValue - the input field's value (model)
       placeholder - placeholder value for empty input
       itemNameChangeMsg - your standard onInput msg (will receive the current value of the input as an arg)
       acceptInputMsg - msg to send when input is committed. Receives the final input as arg.
-}


textInput : String -> String -> (String -> Msg) -> (String -> Msg) -> List (Html Msg)
textInput itemValue placeholder itemNameChangeMsg acceptInputMsg =
    [ input [ type_ "text", value itemValue, Html.Attributes.placeholder placeholder, onInput itemNameChangeMsg ] []
    , button
        [ type_ "button"
        , class "btn btn-success"
        , onClick <| acceptInputMsg itemValue
        , disabled <| itemValue == ""
        ]
        [ text "Ok" ]
    ]


icon : IconSize -> String -> Html Msg
icon size iconName =
    let
        sizeClass =
            case size of
                Small ->
                    "small-icon"

                Normal ->
                    ""
    in
        Html.span [ class <| "material-icons " ++ sizeClass ] [ text iconName ]


projectTasks : Model -> Project -> List Task
projectTasks model project =
    model.tasks
        |> List.filter (\t -> t.projectId == project.id)


getTask : Model -> TaskId -> Maybe Task
getTask model taskId =
    model.tasks
        |> List.filter (\t -> t.id == taskId)
        |> List.head


nounPlural : String -> Int -> String
nounPlural noun count =
    noun
        ++ if count == 1 then
            ""
           else
            "s"


taskTotalTime : Model -> Task -> Time -> Time
taskTotalTime model task time =
    let
        fragmentDuration fragment =
            case fragment.end of
                Just endTime ->
                    endTime - fragment.start

                Nothing ->
                    time - fragment.start
    in
        model.timeFragments
            |> List.filter (\tf -> tf.taskId == task.id)
            |> List.map fragmentDuration
            |> List.sum


getLatestTimeFragment : Model -> Task -> Maybe TimeFragment
getLatestTimeFragment model task =
    let
        fragmentOrderNum =
            (timeFragmentCount model task) - 1
    in
        getTimeFragment model task fragmentOrderNum


getTimeFragment : Model -> Task -> Int -> Maybe TimeFragment
getTimeFragment model task orderNum =
    model.timeFragments
        |> List.filter (\fragment -> fragment.taskId == task.id && fragment.orderNum == orderNum)
        |> List.head


timeFragmentCount : Model -> Task -> Int
timeFragmentCount model task =
    model.timeFragments
        |> List.filter (\fragment -> fragment.taskId == task.id)
        |> List.length



append : a -> List a -> List a
append x =
    flip (++) [ x ]
