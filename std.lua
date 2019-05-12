function table.copy(t)
    local u = {}
    for k, v in pairs(t) do u[k] = v end
    return u
end
function table.add(t, k, v)
    local u = table.copy(t)
    u[k] = v
    return u
end
function table.remove(t, k)
    local u = table.copy(t)
    u[k] = nil
    return u
end
function table.inspect(t)
    for k, v in pairs(t) do print(k, v) end
end
function fix(f)
    return function(...)
        return (function(x) return x(x) end)(function(x) return f(function(y) return x(x)(y) end) end)(...)
    end
end
function file_exists(file)
    local f = io.open(file, "rb")
    if f then f:close() end
    return f ~= nil
end
function lines_from(file)
    if not file_exists(file) then return {} end
    lines = {}
    for line in io.lines(file) do
        lines[#lines + 1] = line
    end
    return lines
end
function prepare_for(x)
    if type(x) == "string" then
        elements = {}
        for i = 1, #x do
            elements[#elements + 1] = x:sub(i,i)
        end
        return elements
    else
        return x
    end
end
function table.slice(tbl, first, last, step)
    local sliced = {}

    for i = first or 1, last or #tbl, step or 1 do
        sliced[#sliced+1] = tbl[i]
    end

    return sliced
end
function table.rev(tbl)
    revved = {}
    for i = #tbl, 1, -1 do
        revved[#revved + 1] = tbl[i]
    end
    return revved
end
function table.cons(tbl, x)
    consed = {x}
    for i = 1, #tbl do
        consed[#consed + 1] = tbl[i]
    end
    return consed
end
