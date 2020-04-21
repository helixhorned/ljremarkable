local error = error
local pcall = pcall
local setfenv = setfenv
local type = type

----------

local api = {}

-- Usage:
--  get_table(loadstring(...))
--  get_table(loadfile(...))
--  get_table(load(...))
function api.get_table(func, loadErrMsg)
    if (func == nil) then
        return nil, "not loadable as Lua code: "..loadErrMsg
    end

    local ok, result = pcall(setfenv(func, {}))

    if (not ok) then
        -- 'result' is an error message.
        return nil, "failed executing in an empty environment: "..result
    elseif (type(result) ~= "table") then
        return nil, "function did not return a table"
    end

    return result
end

-- Done!
return api
