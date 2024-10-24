-- Task Manager Assignment 3 for PLC - CSDS 345
-- Group: Zachary Greenberg, Teddy Bryant, and Daniel Kelly

-- Main function prints welcome and starts taskManager function
main :: IO ()
main = do
    putStrLn "Welcome to Task Manager 3000"
    taskManager []

-- Function that continuously asks user for action to do
taskManager :: [String] -> IO()
taskManager tasks = do
    putStrLn "Below are the options:"
    putStrLn "    add"
    putStrLn "    print"
    putStrLn "    search"
    putStrLn "Enter option: "
    option <- getLine
    -- User can either add, print, or search their tasks, error if action is not one of those
    case option of
        -- Get new task name and re-call taskManager with new list of tasks including new task
        "add" -> do
            putStrLn "Enter Task to Add:"
            newTask <- getLine
            taskManager (newTask : tasks)
        -- Call printTasks function which recursively prints tasks, re-call taskManager
        "print" -> do
            putStrLn "Here are your tasks: "
            printTasks tasks
            taskManager tasks
        -- Call searchTask function which recursively searchs for given task, re-call taskManager
        "search" -> do
            putStrLn "Enter task to search: "
            taskToSearch <- getLine
            searchTask taskToSearch tasks
            taskManager tasks
        -- Other action provided causes error, re-call taskManager
        _ -> do 
            putStrLn "Error"
            taskManager tasks

-- Recursively go through task list and print each task
printTasks :: [String] -> IO()
printTasks [] = return ()
printTasks (t:ts) = do
    putStrLn $ "    " ++ t
    printTasks ts

-- Recursively search for given task in task list, if not found then print that
searchTask :: String -> [String] -> IO ()
searchTask task [] = putStrLn $ "Could not find " ++ task
searchTask task (t:ts)
    | task == t = putStrLn $ "Found " ++ t
    | otherwise = searchTask task ts