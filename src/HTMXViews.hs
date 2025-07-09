{-# LANGUAGE OverloadedStrings #-}

module HTMXViews
  ( todoPage,
    todoList,
    todoItem,
    editTodoForm,
    errorMessage,
  )
where

import Control.Monad (unless)
import Data.Text ()
import Data.Text.Lazy qualified as LT
import Data.Time (defaultTimeLocale, formatTime)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types (Todo (..))

todoPage :: [Todo] -> LT.Text
todoPage todos = renderHtml $ docTypeHtml $ do
  H.head $ do
    H.title "Todo App"
    H.meta ! charset "utf-8"
    H.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.script ! src "https://unpkg.com/htmx.org@1.9.10" $ ""
    H.style $
      toHtml $
        unlines
          [ "body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }",
            ".todo-title { text-align: center; margin-bottom: 30px; }",
            ".todo-item { display: flex; align-items: center; gap: 10px; padding: 10px; border: 1px solid #ddd; margin: 5px 0; border-radius: 5px; }",
            ".todo-item.completed { background-color: #f0f0f0; text-decoration: line-through; }",
            ".todo-form { margin-bottom: 20px; text-align: center; }",
            ".todo-form input[type='text'] { padding: 8px; width: 300px; }",
            ".todo-form button { padding: 8px 16px; background-color: #007bff; color: white; border: none; cursor: pointer; border-radius: 3px; margin-left: 10px; }",
            ".todo-form button:hover { background-color: #0056b3; }",
            ".btn { padding: 4px 8px; margin: 2px; border: none; cursor: pointer; border-radius: 3px; }",
            ".btn-complete { background-color: #28a745; color: white; }",
            ".btn-edit { background-color: #ffc107; color: black; }",
            ".btn-delete { background-color: #dc3545; color: white; }",
            ".btn:hover { opacity: 0.8; }",
            ".error { color: red; padding: 10px; background-color: #ffe6e6; border-radius: 5px; margin: 10px 0; }",
            ".edit-form { display: flex; gap: 10px; align-items: center; }",
            ".edit-form input { padding: 4px; }",
            ".todo-actions { margin-left: auto; }"
            -- , "."
          ]

  body $ do
    H.h1 ! class_ "todo-title" $ "Todo App"

    H.div ! class_ "todo-form" $ do
      H.form
        ! customAttribute "hx-post" "/htmx/todos"
        ! customAttribute "hx-target" "#todo-list"
        ! customAttribute "hx-swap" "outerHTML"
        ! customAttribute "hx-on::after-request" "if(event.detail.successful) this.reset()"
        $ do
          H.input
            ! type_ "text"
            ! name "title"
            ! placeholder "Add new todo..."
            ! required ""
          H.button ! type_ "submit" $ "Add Todo"

    todoListHtml todos

todoList :: [Todo] -> LT.Text
todoList todos = renderHtml $ todoListHtml todos

todoListHtml :: [Todo] -> Html
todoListHtml todos = H.div ! A.id "todo-list" $ do
  if null todos
    then H.p "No todos yet!"
    else mapM_ todoItemHtml todos

todoItem :: Todo -> LT.Text
todoItem todo = renderHtml $ todoItemHtml todo

todoItemHtml :: Todo -> Html
todoItemHtml todo = H.div
  ! A.id (toValue $ "todo-" <> show (todoId todo))
  ! class_ (if todoCompleted todo then "todo-item completed" else "todo-item")
  $ do
    H.div ! class_ "todo-content" $ do
      H.span ! class_ "todo-title" $ toHtml $ todoTitle todo
      H.small $ toHtml $ " (created: " <> formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (todoCreatedAt todo) <> ")"

    H.div ! class_ "todo-actions" $ do
      unless (todoCompleted todo)
        $ H.button
          ! class_ "btn btn-complete"
          ! customAttribute "hx-post" (toValue $ "/htmx/todos/" <> show (todoId todo) <> "/toggle")
          ! customAttribute "hx-target" "#todo-list"
          ! customAttribute "hx-swap" "outerHTML"
        $ "Complete"

      H.button
        ! class_ "btn btn-edit"
        ! customAttribute "hx-get" (toValue $ "/htmx/todos/" <> show (todoId todo) <> "/edit")
        ! customAttribute "hx-target" (toValue $ "#todo-" <> show (todoId todo))
        ! customAttribute "hx-swap" "outerHTML"
        $ "Edit"

      H.button
        ! class_ "btn btn-delete"
        ! customAttribute "hx-delete" (toValue $ "/htmx/todos/" <> show (todoId todo))
        ! customAttribute "hx-target" "#todo-list"
        ! customAttribute "hx-swap" "outerHTML"
        ! customAttribute "hx-confirm" "Are you sure you want to delete this todo?"
        $ "Delete"

editTodoForm :: Todo -> LT.Text
editTodoForm todo = renderHtml
  $ H.div
    ! A.id (toValue $ "todo-" <> show (todoId todo))
    ! class_ "todo-item"
  $ do
    H.form
      ! class_ "edit-form"
      ! customAttribute "hx-put" (toValue $ "/htmx/todos/" <> show (todoId todo))
      ! customAttribute "hx-target" "#todo-list"
      ! customAttribute "hx-swap" "outerHTML"
      $ do
        H.input ! type_ "text" ! name "title" ! value (toValue $ todoTitle todo) ! required ""
        H.button ! type_ "submit" ! class_ "btn btn-complete" $ "Save"
        H.button
          ! type_ "button"
          ! class_ "btn btn-delete"
          ! customAttribute "hx-get" "/htmx/todos"
          ! customAttribute "hx-target" "#todo-list"
          ! customAttribute "hx-swap" "outerHTML"
          $ "Cancel"

errorMessage :: String -> LT.Text
errorMessage msg = renderHtml $ H.div ! class_ "error" $ toHtml msg