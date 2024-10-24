main :: IO ()
main = taskManager []

taskManager :: [String] -> IO()
taskManager tasks = do
    putStrLn "Task Manager running"
    putStrLn "Possible Options:"
    putStrLn "add"
    putStrLn "print"
    putStrLn "search"
    putStrLn "Please select an option: "
    option <- getLine
    case option of
        "add" -> do
            putStrLn "What task do you want to add?"
            newTask <- getLine
            taskManager (newTask : tasks)
        "print" -> do
            putStrLn "Here are your tasks to do: "
            printTasks tasks
            taskManager tasks
        "search" -> do
            putStrLn "Enter task to search: "
            taskToSearch <- getLine
            searchTask taskToSearch tasks
            taskManager tasks
        _ -> do 
            putStrLn "Error"

printTasks :: [String] -> IO()
printTasks [] = return ()
printTasks (t:ts) = do
    putStrLn t
    printTasks ts

searchTask :: String -> [String] -> IO ()
searchTask_ [] = putStrLn "Task was not found"
searchTask task (t:ts)
    | task == t = putStrLn $ "Found: " ++ t
    | otherwise = searchTask task ts