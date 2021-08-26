-- SPDX-License-Identifier: MIT

local assert = assert
local error = error
local pcall = pcall
local loadfile = loadfile
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

function api.get_AZ_map(layoutFileName)
    local syms, msg = api.get_table(loadfile(layoutFileName))
    if (syms == nil) then
        error("Failed loading layout: "..msg)
    end

    local TotalDestKeyCount = 40
    local KeyIdxFactor = 10

    local count = 0
    local map = {}

    for k = 1, TotalDestKeyCount do
        local sym = syms[KeyIdxFactor * k]

        if (sym ~= nil and sym:match("^[a-z]$")) then
            local upperSym = sym:upper()
            if (map[upperSym] ~= nil) then
                error("Ambiguous key number for symbol "..upperSym)
            end

            map[upperSym] = k
            count = count + 1
        end
    end

    assert(count == 26, "Unexpected final count of symbols "..count)

    return map
end

-- Done!
return api
