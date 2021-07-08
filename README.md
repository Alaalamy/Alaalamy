- 👋 Hi, I’m @Alaalamy
- 👀 I’m interested in ...
- 🌱 I’m currently learning ...
- 💞️ I’m looking to collaborate on ...
- 📫 How to reach me ...

<!---
Alaalamy/Alaalamy is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
-- Module options:
    local always_try_using_lpeg = true
    local register_global_module_table = false
    local global_module_name =  json 

    --[==[
David Kolf s JSON module for Lua 5.1/5.2
========================================
*Version 2.4*
In the default configuration this module writes no global values, not even
the module table. Import it using
    json = require ("dkjson")
In environments where `require` or a similiar function are not available
and you cannot receive the return value of the module, you can set the
option `register_global_module_table` to `true`.  The module table will
then be saved in the global variable with the name given by the option
`global_module_name`.
Exported functions and values:
`json.encode (object [, state])`
--------------------------------
Create a string representing the object. `Object` can be a table,
a string, a number, a boolean, `nil`, `json.null` or any object with
a function `__tojson` in its metatable. A table can only use strings
and numbers as keys and its values have to be valid objects as
well. It raises an error for any invalid data types or reference
cycles.
`state` is an optional table with the following fields:
  - `indent`  
    When `indent` (a boolean) is set, the created string will contain
    newlines and indentations. Otherwise it will be one long line.
  - `keyorder`  
    `keyorder` is an array to specify the ordering of keys in the
    encoded output. If an object has keys which are not in this array
    they are written after the sorted keys.
  - `level`  
    This is the initial level of indentation used when `indent` is
    set. For each level two spaces are added. When absent it is set
    to 0.
  - `buffer`  
    `buffer` is an array to store the strings for the result so they
    can be concatenated at once. When it isn t given, the encode
    function will create it temporary and will return the
    concatenated result.
  - `bufferlen`  
    When `bufferlen` is set, it has to be the index of the last
    element of `buffer`.
  - `tables`  
    `tables` is a set to detect reference cycles. It is created
    temporary when absent. Every table that is currently processed
    is used as key, the value is `true`.
When `state.buffer` was set, the return value will be `true` on
success. Without `state.buffer` the return value will be a string.
`json.decode (string [, position [, null]])`
--------------------------------------------
Decode `string` starting at `position` or at 1 if `position` was
omitted.
`null` is an optional value to be returned for null values. The
default is `nil`, but you could set it to `json.null` or any other
value.
The return values are the object or `nil`, the position of the next
character that doesn t belong to the object, and in case of errors
an error message.
Two metatables are created. Every array or object that is decoded gets
a metatable with the `__jsontype` field set to either `array` or
`object`. If you want to provide your own metatables use the syntax
    json.decode (string, position, null, objectmeta, arraymeta)
To prevent the assigning of metatables pass `nil`:
    json.decode (string, position, null, nil)
`<metatable>.__jsonorder`
-------------------------
`__jsonorder` can overwrite the `keyorder` for a specific table.
`<metatable>.__jsontype`
------------------------
`__jsontype` can be either `"array"` or `"object"`. This value is only
checked for empty tables. (The default for empty tables is `"array"`).
`<metatable>.__tojson (self, state)`
------------------------------------
You can provide your own `__tojson` function in a metatable. In this
function you can either add directly to the buffer and return true,
or you can return a string. On errors nil and a message should be
returned.
`json.null`
-----------
You can use this value for setting explicit `null` values.
`json.version`
--------------
Set to `"dkjson 2.4"`.
`json.quotestring (string)`
---------------------------
Quote a UTF-8 string and escape critical characters using JSON
escape sequences. This function is only necessary when you build
your own `__tojson` functions.
`json.addnewline (state)`
-------------------------
When `state.indent` is set, add a newline to `state.buffer` and spaces
according to `state.level`.
LPeg support
------------
When the local configuration variable `always_try_using_lpeg` is set,
this module tries to load LPeg to replace the `decode` function. The
speed increase is significant. You can get the LPeg module at
  <http://www.inf.puc-rio.br/~roberto/lpeg/>.
When LPeg couldn t be loaded, the pure Lua functions stay active.
In case you don t want this module to require LPeg on its own,
disable the option `always_try_using_lpeg` in the options section at
the top of the module.
In this case you can later load LPeg support using
### `json.use_lpeg ()`
Require the LPeg module and replace the functions `quotestring` and
and `decode` with functions that use LPeg patterns.
This function returns the module table, so you can load the module
using:
    json = require "dkjson".use_lpeg()
Alternatively you can use `pcall` so the JSON module still works when
LPeg isn t found.
    json = require "dkjson"
    pcall (json.use_lpeg)
### `json.using_lpeg`
This variable is set to `true` when LPeg was loaded successfully.
---------------------------------------------------------------------
Contact
-------
You can contact the author by sending an e-mail to  david  at the
domain  dkolf.de .
---------------------------------------------------------------------
*Copyright (C) 2010-2013 David Heiko Kolf*
Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:
The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
<!-- This documentation can be parsed using Markdown to generate HTML.
     The source code is enclosed in a HTML comment so it won t be displayed
     by browsers, but it should be removed from the final HTML file as
     it isn t a valid HTML comment (and wastes space).
  -->
<!--]==]

-- global dependencies:
local pairs, type, tostring, tonumber, getmetatable, setmetatable, rawset =
      pairs, type, tostring, tonumber, getmetatable, setmetatable, rawset
local error, require, pcall, select = error, require, pcall, select
local floor, huge = math.floor, math.huge
local strrep, gsub, strsub, strbyte, strchar, strfind, strlen, strformat =
      string.rep, string.gsub, string.sub, string.byte, string.char,
      string.find, string.len, string.format
local strmatch = string.match
local concat = table.concat

local json = { version = "dkjson 2.4" }

if register_global_module_table then
  _G[global_module_name] = json
end

local _ENV = nil -- blocking globals in Lua 5.2

pcall (function()
  -- Enable access to blocked metatables.
  -- Don t worry, this module doesn t change anything in them.
  local debmeta = require "debug".getmetatable
  if debmeta then getmetatable = debmeta end
end)

json.null = setmetatable ({}, {
  __tojson = function () return "null" end
})

local function isarray (tbl)
  local max, n, arraylen = 0, 0, 0
  for k,v in pairs (tbl) do
    if k ==  n  and type(v) ==  number  then
      arraylen = v
      if v > max then
        max = v
      end
    else
      if type(k) ~=  number  or k < 1 or floor(k) ~= k then
        return false
      end
      if k > max then
        max = k
      end
      n = n + 1
    end
  end
  if max > 10 and max > arraylen and max > n * 2 then
    return false -- don t create an array with too many holes
  end
  return true, max
end

local escapecodes = {
  ["\""] = "\\\"", ["\\"] = "\\\\", ["\b"] = "\\b", ["\f"] = "\\f",
  ["\n"] = "\\n",  ["\r"] = "\\r",  ["\t"] = "\\t"
}

local function escapeutf8 (uchar)
  local value = escapecodes[uchar]
  if value then
    return value
  end
  local a, b, c, d = strbyte (uchar, 1, 4)
  a, b, c, d = a or 0, b or 0, c or 0, d or 0
  if a <= 0x7f then
    value = a
  elseif 0xc0 <= a and a <= 0xdf and b >= 0x80 then
    value = (a - 0xc0) * 0x40 + b - 0x80
  elseif 0xe0 <= a and a <= 0xef and b >= 0x80 and c >= 0x80 then
    value = ((a - 0xe0) * 0x40 + b - 0x80) * 0x40 + c - 0x80
  elseif 0xf0 <= a and a <= 0xf7 and b >= 0x80 and c >= 0x80 and d >= 0x80 then
    value = (((a - 0xf0) * 0x40 + b - 0x80) * 0x40 + c - 0x80) * 0x40 + d - 0x80
  else
    return ""
  end
  if value <= 0xffff then
    return strformat ("\\u%.4x", value)
  elseif value <= 0x10ffff then
    -- encode as UTF-16 surrogate pair
    value = value - 0x10000
    local highsur, lowsur = 0xD800 + floor (value/0x400), 0xDC00 + (value % 0x400)
    return strformat ("\\u%.4x\\u%.4x", highsur, lowsur)
  else
    return ""
  end
end

local function fsub (str, pattern, repl)
  -- gsub always builds a new string in a buffer, even when no match
  -- exists. First using find should be more efficient when most strings
  -- don t contain the pattern.
  if strfind (str, pattern) then
    return gsub (str, pattern, repl)
  else
    return str
  end
end

local function quotestring (value)
  -- based on the regexp "escapable" in https://github.com/douglascrockford/JSON-js
  value = fsub (value, "[%z\1-\31\"\\\127]", escapeutf8)
  if strfind (value, "[\194\216\220\225\226\239]") then
    value = fsub (value, "\194[\128-\159\173]", escapeutf8)
    value = fsub (value, "\216[\128-\132]", escapeutf8)
    value = fsub (value, "\220\143", escapeutf8)
    value = fsub (value, "\225\158[\180\181]", escapeutf8)
    value = fsub (value, "\226\128[\140-\143\168-\175]", escapeutf8)
    value = fsub (value, "\226\129[\160-\175]", escapeutf8)
    value = fsub (value, "\239\187\191", escapeutf8)
    value = fsub (value, "\239\191[\176-\191]", escapeutf8)
  end
  return "\"" .. value .. "\""
end
json.quotestring = quotestring

local function replace(str, o, n)
  local i, j = strfind (str, o, 1, true)
  if i then
    return strsub(str, 1, i-1) .. n .. strsub(str, j+1, -1)
  else
    return str
  end
end

-- locale independent num2str and str2num functions
local decpoint, numfilter

local function updatedecpoint ()
  decpoint = strmatch(tostring(0.5), "([^05+])")
  -- build a filter that can be used to remove group separators
  numfilter = "[^0-9%-%+eE" .. gsub(decpoint, "[%^%$%(%)%%%.%[%]%*%+%-%?]", "%%%0") .. "]+"
end

updatedecpoint()

local function num2str (num)
  return replace(fsub(tostring(num), numfilter, ""), decpoint, ".")
end

local function str2num (str)
  local num = tonumber(replace(str, ".", decpoint))
  if not num then
    updatedecpoint()
    num = tonumber(replace(str, ".", decpoint))
  end
  return num
end

local function addnewline2 (level, buffer, buflen)
  buffer[buflen+1] = "\n"
  buffer[buflen+2] = strrep ("  ", level)
  buflen = buflen + 2
  return buflen
end

function json.addnewline (state)
  if state.indent then
    state.bufferlen = addnewline2 (state.level or 0,
                           state.buffer, state.bufferlen or #(state.buffer))
  end
end

local encode2 -- forward declaration

local function addpair (key, value, prev, indent, level, buffer, buflen, tables, globalorder)
  local kt = type (key)
  if kt ~=  string  and kt ~=  number  then
    return nil, "type  " .. kt .. "  is not supported as a key by JSON."
  end
  if prev then
    buflen = buflen + 1
    buffer[buflen] = ","
  end
  if indent then
    buflen = addnewline2 (level, buffer, buflen)
  end
  buffer[buflen+1] = quotestring (key)
  buffer[buflen+2] = ":"
  return encode2 (value, indent, level, buffer, buflen + 2, tables, globalorder)
end

encode2 = function (value, indent, level, buffer, buflen, tables, globalorder)
  local valtype = type (value)
  local valmeta = getmetatable (value)
  valmeta = type (valmeta) ==  table  and valmeta -- only tables
  local valtojson = valmeta and valmeta.__tojson
  if valtojson then
    if tables[value] then
      return nil, "reference cycle"
    end
    tables[value] = true
    local state = {
        indent = indent, level = level, buffer = buffer,
        bufferlen = buflen, tables = tables, keyorder = globalorder
    }
    local ret, msg = valtojson (value, state)
    if not ret then return nil, msg end
    tables[value] = nil
    buflen = state.bufferlen
    if type (ret) ==  string  then
      buflen = buflen + 1
      buffer[buflen] = ret
    end
  elseif value == nil then
    buflen = buflen + 1
    buffer[buflen] = "null"
  elseif valtype ==  number  then
    local s
    if value ~= value or value >= huge or -value >= huge then
      -- This is the behaviour of the original JSON implementation.
      s = "null"
    else
      s = num2str (value)
    end
    buflen = buflen + 1
    buffer[buflen] = s
  elseif valtype ==  boolean  then
    buflen = buflen + 1
    buffer[buflen] = value and "true" or "false"
  elseif valtype ==  string  then
    buflen = buflen + 1
    buffer[buflen] = quotestring (value)
  elseif valtype ==  table  then
    if tables[value] then
      return nil, "reference cycle"
    end
    tables[value] = true
    level = level + 1
    local isa, n = isarray (value)
    if n == 0 and valmeta and valmeta.__jsontype ==  object  then
      isa = false
    end
    local msg
    if isa then -- JSON array
      buflen = buflen + 1
      buffer[buflen] = "["
      for i = 1, n do
        buflen, msg = encode2 (value[i], indent, level, buffer, buflen, tables, globalorder)
        if not buflen then return nil, msg end
        if i < n then
          buflen = buflen + 1
          buffer[buflen] = ","
        end
      end
      buflen = buflen + 1
      buffer[buflen] = "]"
    else -- JSON object
      local prev = false
      buflen = buflen + 1
      buffer[buflen] = "{"
      local order = valmeta and valmeta.__jsonorder or globalorder
      if order then
        local used = {}
        n = #order
        for i = 1, n do
          local k = order[i]
          local v = value[k]
          if v then
            used[k] = true
            buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder)
            prev = true -- add a seperator before the next element
          end
        end
        for k,v in pairs (value) do
          if not used[k] then
            buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder)
            if not buflen then return nil, msg end
            prev = true -- add a seperator before the next element
          end
        end
      else -- unordered
        for k,v in pairs (value) do
          buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder)
          if not buflen then return nil, msg end
          prev = true -- add a seperator before the next element
        end
      end
      if indent then
        buflen = addnewline2 (level - 1, buffer, buflen)
      end
      buflen = buflen + 1
      buffer[buflen] = "}"
    end
    tables[value] = nil
  else
    return nil, "type  " .. valtype .. "  is not supported by JSON."
  end
  return buflen
end

function json.encode (value, state)
  state = state or {}
  local oldbuffer = state.buffer
  local buffer = oldbuffer or {}
  updatedecpoint()
  local ret, msg = encode2 (value, state.indent, state.level or 0,
                   buffer, state.bufferlen or 0, state.tables or {}, state.keyorder)
  if not ret then
    error (msg, 2)
  elseif oldbuffer then
    state.bufferlen = ret
    return true
  else
    return concat (buffer)
  end
end

local function loc (str, where)
  local line, pos, linepos = 1, 1, 0
  while true do
    pos = strfind (str, "\n", pos, true)
    if pos and pos < where then
      line = line + 1
      linepos = pos
      pos = pos + 1
    else
      break
    end
  end
  return "line " .. line .. ", column " .. (where - linepos)
end

local function unterminated (str, what, where)
  return nil, strlen (str) + 1, "unterminated " .. what .. " at " .. loc (str, where)
end

local function scanwhite (str, pos)
  while true do
    pos = strfind (str, "%S", pos)
    if not pos then return nil end
    if strsub (str, pos, pos + 2) == "\239\187\191" then
      -- UTF-8 Byte Order Mark
      pos = pos + 3
    else
      return pos
    end
  end
end

local escapechars = {
  ["\""] = "\"", ["\\"] = "\\", ["/"] = "/", ["b"] = "\b", ["f"] = "\f",
  ["n"] = "\n", ["r"] = "\r", ["t"] = "\t"
}

local function unichar (value)
  if value < 0 then
    return nil
  elseif value <= 0x007f then
    return strchar (value)
  elseif value <= 0x07ff then
    return strchar (0xc0 + floor(value/0x40),
                    0x80 + (floor(value) % 0x40))
  elseif value <= 0xffff then
    return strchar (0xe0 + floor(value/0x1000),
                    0x80 + (floor(value/0x40) % 0x40),
                    0x80 + (floor(value) % 0x40))
  elseif value <= 0x10ffff then
    return strchar (0xf0 + floor(value/0x40000),
                    0x80 + (floor(value/0x1000) % 0x40),
                    0x80 + (floor(value/0x40) % 0x40),
                    0x80 + (floor(value) % 0x40))
  else
    return nil
  end
end

local function scanstring (str, pos)
  local lastpos = pos + 1
  local buffer, n = {}, 0
  while true do
    local nextpos = strfind (str, "[\"\\]", lastpos)
    if not nextpos then
      return unterminated (str, "string", pos)
    end
    if nextpos > lastpos then
      n = n + 1
      buffer[n] = strsub (str, lastpos, nextpos - 1)
    end
    if strsub (str, nextpos, nextpos) == "\"" then
      lastpos = nextpos + 1
      break
    else
      local escchar = strsub (str, nextpos + 1, nextpos + 1)
      local value
      if escchar == "u" then
        value = tonumber (strsub (str, nextpos + 2, nextpos + 5), 16)
        if value then
          local value2
          if 0xD800 <= value and value <= 0xDBff then
            -- we have the high surrogate of UTF-16. Check if there is a
            -- low surrogate escaped nearby to combine them.
            if strsub (str, nextpos + 6, nextpos + 7) == "\\u" then
              value2 = tonumber (strsub (str, nextpos + 8, nextpos + 11), 16)
              if value2 and 0xDC00 <= value2 and value2 <= 0xDFFF then
                value = (value - 0xD800)  * 0x400 + (value2 - 0xDC00) + 0x10000
              else
                value2 = nil -- in case it was out of range for a low surrogate
              end
            end
          end
          value = value and unichar (value)
          if value then
            if value2 then
              lastpos = nextpos + 12
            else
              lastpos = nextpos + 6
            end
          end
        end
      end
      if not value then
        value = escapechars[escchar] or escchar
        lastpos = nextpos + 2
      end
      n = n + 1
      buffer[n] = value
    end
  end
  if n == 1 then
    return buffer[1], lastpos
  elseif n > 1 then
    return concat (buffer), lastpos
  else
    return "", lastpos
  end
end

local scanvalue -- forward declaration

local function scantable (what, closechar, str, startpos, nullval, objectmeta, arraymeta)
  local len = strlen (str)
  local tbl, n = {}, 0
  local pos = startpos + 1
  if what ==  object  then
    setmetatable (tbl, objectmeta)
  else
    setmetatable (tbl, arraymeta)
  end
  while true do
    pos = scanwhite (str, pos)
    if not pos then return unterminated (str, what, startpos) end
    local char = strsub (str, pos, pos)
    if char == closechar then
      return tbl, pos + 1
    end
    local val1, err
    val1, pos, err = scanvalue (str, pos, nullval, objectmeta, arraymeta)
    if err then return nil, pos, err end
    pos = scanwhite (str, pos)
    if not pos then return unterminated (str, what, startpos) end
    char = strsub (str, pos, pos)
    if char == ":" then
      if val1 == nil then
        return nil, pos, "cannot use nil as table index (at " .. loc (str, pos) .. ")"
      end
      pos = scanwhite (str, pos + 1)
      if not pos then return unterminated (str, what, startpos) end
      local val2
      val2, pos, err = scanvalue (str, pos, nullval, objectmeta, arraymeta)
      if err then return nil, pos, err end
      tbl[val1] = val2
      pos = scanwhite (str, pos)
      if not pos then return unterminated (str, what, startpos) end
      char = strsub (str, pos, pos)
    else
      n = n + 1
      tbl[n] = val1
    end
    if char == "," then
      pos = pos + 1
    end
  end
end

scanvalue = function (str, pos, nullval, objectmeta, arraymeta)
  pos = pos or 1
  pos = scanwhite (str, pos)
  if not pos then
    return nil, strlen (str) + 1, "no valid JSON value (reached the end)"
  end
  local char = strsub (str, pos, pos)
  if char == "{" then
    return scantable ( object , "}", str, pos, nullval, objectmeta, arraymeta)
  elseif char == "[" then
    return scantable ( array , "]", str, pos, nullval, objectmeta, arraymeta)
  elseif char == "\"" then
    return scanstring (str, pos)
  else
    local pstart, pend = strfind (str, "^%-?[%d%.]+[eE]?[%+%-]?%d*", pos)
    if pstart then
      local number = str2num (strsub (str, pstart, pend))
      if number then
        return number, pend + 1
      end
    end
    pstart, pend = strfind (str, "^%a%w*", pos)
    if pstart then
      local name = strsub (str, pstart, pend)
      if name == "true" then
        return true, pend + 1
      elseif name == "false" then
        return false, pend + 1
      elseif name == "null" then
        return nullval, pend + 1
      end
    end
    return nil, pos, "no valid JSON value at " .. loc (str, pos)
  end
end

local function optionalmetatables(...)
  if select("#", ...) > 0 then
    return ...
  else
    return {__jsontype =  object }, {__jsontype =  array }
  end
end

function json.decode (str, pos, nullval, ...)
  local objectmeta, arraymeta = optionalmetatables(...)
  return scanvalue (str, pos, nullval, objectmeta, arraymeta)
end

function json.use_lpeg ()
  local g = require ("lpeg")

  if g.version() == "0.11" then
    error "due to a bug in LPeg 0.11, it cannot be used for JSON matching"
  end

  local pegmatch = g.match
  local P, S, R = g.P, g.S, g.R

  local function ErrorCall (str, pos, msg, state)
    if not state.msg then
      state.msg = msg .. " at " .. loc (str, pos)
      state.pos = pos
    end
    return false
  end

  local function Err (msg)
    return g.Cmt (g.Cc (msg) * g.Carg (2), ErrorCall)
  end

  local Space = (S" \n\r\t" + P"\239\187\191")^0

  local PlainChar = 1 - S"\"\\\n\r"
  local EscapeSequence = (P"\\" * g.C (S"\"\\/bfnrt" + Err "unsupported escape sequence")) / escapechars
  local HexDigit = R("09", "af", "AF")
  local function UTF16Surrogate (match, pos, high, low)
    high, low = tonumber (high, 16), tonumber (low, 16)
    if 0xD800 <= high and high <= 0xDBff and 0xDC00 <= low and low <= 0xDFFF then
      return true, unichar ((high - 0xD800)  * 0x400 + (low - 0xDC00) + 0x10000)
    else
      return false
    end
  end
  local function UTF16BMP (hex)
    return unichar (tonumber (hex, 16))
  end
  local U16Sequence = (P"\\u" * g.C (HexDigit * HexDigit * HexDigit * HexDigit))
  local UnicodeEscape = g.Cmt (U16Sequence * U16Sequence, UTF16Surrogate) + U16Sequence/UTF16BMP
  local Char = UnicodeEscape + EscapeSequence + PlainChar
  local String = P"\"" * g.Cs (Char ^ 0) * (P"\"" + Err "unterminated string")
  local Integer = P"-"^(-1) * (P"0" + (R"19" * R"09"^0))
  local Fractal = P"." * R"09"^0
  local Exponent = (S"eE") * (S"+-")^(-1) * R"09"^1
  local Number = (Integer * Fractal^(-1) * Exponent^(-1))/str2num
  local Constant = P"true" * g.Cc (true) + P"false" * g.Cc (false) + P"null" * g.Carg (1)
  local SimpleValue = Number + String + Constant
  local ArrayContent, ObjectContent

  -- The functions parsearray and parseobject parse only a single value/pair
  -- at a time and store them directly to avoid hitting the LPeg limits.
  local function parsearray (str, pos, nullval, state)
    local obj, cont
    local npos
    local t, nt = {}, 0
    repeat
      obj, cont, npos = pegmatch (ArrayContent, str, pos, nullval, state)
      if not npos then break end
      pos = npos
      nt = nt + 1
      t[nt] = obj
    until cont ==  last 
    return pos, setmetatable (t, state.arraymeta)
  end

  local function parseobject (str, pos, nullval, state)
    local obj, key, cont
    local npos
    local t = {}
    repeat
      key, obj, cont, npos = pegmatch (ObjectContent, str, pos, nullval, state)
      if not npos then break end
      pos = npos
      t[key] = obj
    until cont ==  last 
    return pos, setmetatable (t, state.objectmeta)
  end

  local Array = P"[" * g.Cmt (g.Carg(1) * g.Carg(2), parsearray) * Space * (P"]" + Err " ]  expected")
  local Object = P"{" * g.Cmt (g.Carg(1) * g.Carg(2), parseobject) * Space * (P"}" + Err " }  expected")
  local Value = Space * (Array + Object + SimpleValue)
  local ExpectedValue = Value + Space * Err "value expected"
  ArrayContent = Value * Space * (P"," * g.Cc cont  + g.Cc last ) * g.Cp()
  local Pair = g.Cg (Space * String * Space * (P":" + Err "colon expected") * ExpectedValue)
  ObjectContent = Pair * Space * (P"," * g.Cc cont  + g.Cc last ) * g.Cp()
  local DecodeValue = ExpectedValue * g.Cp ()

  function json.decode (str, pos, nullval, ...)
    local state = {}
    state.objectmeta, state.arraymeta = optionalmetatables(...)
    local obj, retpos = pegmatch (DecodeValue, str, pos, nullval, state)
    if state.msg then
      return nil, state.pos, state.msg
    else
      return obj, retpos
    end
  end

  -- use this function only once:
  json.use_lpeg = function () return json end

  json.using_lpeg = true

  return json -- so you can get the module using json = require "dkjson".use_lpeg()
end

if always_try_using_lpeg then
  pcall (json.use_lpeg)
end

return json
---> 
 ألعاب الوظيفة  المحلية ( msg )
نص محلي = msg. المحتوى_ . نص_
إذا نص ==    $$ سم مكعب "  و قاعدة البيانات: الحصول على (bot_id .. " قفل: ألعاب " .. جي اس. chat_id_ ) ثم
إذا كان  AddChannel (msg. sender_user_id_ ) ==  false  إذًا
local textchuser = قاعدة البيانات: get (bot_id ..   text: ch: user   )
إذا textchuser ثم
إرسال (msg. chat_id_ ، msg. id_ ،   [   .. textchuser ..   ]   )
آخر
send (msg. chat_id_ ، msg. id_ ،    تستطيع استخدام البوت \ n   ✯يرجى بخدمة بالقناه اولا \ n   ✯اشترك هنا [   .. قاعدة بيانات: get (bot_id ..   add: ch: username   ) ..   ]   )
نهاية
عودة  كاذبة
نهاية
Text_Games =  [[
]]
إرسال (msg. chat_id_ ، msg. id_ ، Text_Games)
نهاية
إذا كان النص ==    $$$ ccc    أو text ==    $$ cc    إذن
نص محلي =  [[
]]
لوحة المفاتيح = {}
لوحة المفاتيح. inline_keyboard  = {
{{text =    𝒔𝒐𝒖𝒓𝒄𝒆 𝒔𝒐𝒐𝒐𝒏   ، url = " t.me/s_o_op " }}،
}
msg_id المحلي = msg. معرف_ / 2097152 / 0.5
https. request ( " https://api.telegram.org/bot " .. رمز مميز ..   / sendMessage؟ chat_id =    .. msg. chat_id_  ..    & text =    .. URL. escape (Text) .. " & reply_to_message_id = " .. msg_id .. " & parse_mode = markdown & disable_web_page_preview = true & reply_markup = " .. تشفير JSON (لوحة المفاتيح))
نهاية
إذا كان النص ==    السمايلات    أو text ==    السمايل    ثم
إذا كان  AddChannel (msg. sender_user_id_ ) ==  false  إذًا
local textchuser = قاعدة البيانات: get (bot_id ..   text: ch: user   )
إذا textchuser ثم
إرسال (msg. chat_id_ ، msg. id_ ،   [   .. textchuser ..   ]   )
آخر
send (msg. chat_id_ ، msg. id_ ،    تستطيع استخدام البوت \ n   ✯يرجى بخدمة بالقناه اولا \ n   ✯اشترك هنا [   .. قاعدة بيانات: get (bot_id ..   add: ch: username   ) ..   ]   )
نهاية
عودة  كاذبة
نهاية
إذا كانت قاعدة البيانات: احصل على (bot_id ..   Lock: Games   .. msg. chat_id_ ) ثم
قاعدة البيانات: del (bot_id ..   Set: Sma   .. msg. chat_id_ )
عشوائية = {   🍏   ،   🍎   ،   🍐   ،   🍊   ،   ؟؟   ،   🍉   ،   🍇   ،   🍓   ،   🍈   ،   🍒   ،   🍑   ،   🍍   ،   ؟؟ " ، "" 🍅   ،   🍆   ،   🥑   ،   🥦   ،   🥒   ،   🌶   ،   🌽   ،   🥕   ،   🥔   ،   🥖   ،   🥐   ،   🍞   ،   🥨   ،   🍟   ،  🧀   ،" 🥚   ،   🍳   ،   🥓   ،   🥩   ،   🍗   ،   🍖   ،   🌭   ،   🍔   ،   🍠   ،   🍕   ،   🥪   ،   🥙   ،   ☕️   ،   🍵   ،  🥤   ،" 🍶   ،   🍺   ،   🍻   ،   🏀   ،   ⚽️   ،   🏈   ،   ⚾️   ،   🎾   ،   🏐   ،   🏉   ،   🎱   ،   🏓   ،   🏸   ،   🥅   ،  🎰   ،" 🎮   ،   🎳   ،   🎯   ،   🎲   ،   🎻   ،   🎸   ،   🎺   ،   🥁   ،   🎹   ،   🎼   ،   🎧   ،   🎤   ،   🎬   ،   🎨   ،  🎭   ،" 🎪   ،   🎟   ،   🎫   ،   🎗   ،   🏵   ،   🎖   ،   🏆   ،   🥌   ،   🛷   ،   🚗   ،   🚌   ،   🏎   ،   🚓   ،   🚑   ،  🚚   ،" 🚛   ،   🚜   ،   🇮🇶   ،   ⚔   ،   🛡   ،   🔮   ،   🌡   ،   💣   ،   📌   ،   📍   ،   📓   ،   📗   ،   📂   ،   📅   ،  📪   ،" 📫   ،   📬   ،   📭   ،   ⏰   ،   📺   ،   🎚   ،   ☎️   ،   📡 " }
SM = عشوائي [ math.random ( # عشوائي)]
قاعدة البيانات: مجموعة (bot_id ..   عشوائي: Sm   .. msg. chat_id_ ، SM)
إرسال (جي اس. chat_id_ ، جي اس. id_ ،   ✯اسرع واحد يدز هاذا السمايل؟ »{`   .. SM ..   `} " )
عودة  كاذبة
نهاية
نهاية
إذا نص ==      .. (قاعدة بيانات: الحصول على (bot_id .. " عشوائية: ن خ   .. . جي اس chat_id_ ) أو      ) ..      و  ليس قاعدة البيانات: الحصول على (bot_id ..   مجموعة: سعد محمد " . . جي اس. chat_id_ ) ثم
إذا  لم تكن قاعدة البيانات: احصل على (bot_id ..   Set: Sma   .. msg. chat_id_ ) ثم 
send (msg. chat_id_ ، msg. id_ ،   ✯الف مبروك لقد فزت \ n ✯للعب مره اخره ارسل »{سمايل، سمايلات}   )
قاعدة البيانات: incrby (bot_id ..   NUM: GAMES   .. msg. chat_id_ .. msg. sender_user_id_ ، 1 )  
نهاية
قاعدة البيانات: set (bot_id ..   Set: Sma   .. msg. chat_id_ ، true )
عودة  كاذبة
نهاية 
إذا كان النص ==    اسرع    أو نص ==    الاسرع    أو نص ==    ترتيب    ثم
إذا كان  AddChannel (msg. sender_user_id_ ) ==  false  إذًا
local textchuser = قاعدة البيانات: get (bot_id ..   text: ch: user   )
إذا textchuser ثم
إرسال (msg. chat_id_ ، msg. id_ ،   [   .. textchuser ..   ]   )
آخر
send (msg. chat_id_ ، msg. id_ ،    تستطيع استخدام البوت \ n   ✯يرجى بخدمة بالقناه اولا \ n   ✯اشترك هنا [   .. قاعدة بيانات: get (bot_id ..   add: ch: username   ) ..   ]   )
نهاية
عودة  كاذبة
نهاية
إذا كانت قاعدة البيانات: احصل على (bot_id ..   Lock: Games   .. msg. chat_id_ ) ثم
قاعدة البيانات: del (bot_id ..   Speed: Tr   .. msg. chat_id_ )
KlamSpeed = {   سحور   ،   سياره   ،   استقبال   ،   قنفه   ،   ايفون   ،   بزونه   ،   مطبخ   ،   كرستيانو   ،   دجاجه   ،   مدرسه   ،   الوان   ،   غرفه   ،   ثلاجه "،  كهوه   ،   سفينه   ،   العراق   ،   محطه   ،   طياره   ،   رادار   ،   منزل   ،   مستشفى   ،   كهرباء   ،   تفاحه " ، " اخطبوط " ، " سلمون " ، " فرنسا " ،   برتقاله "،   تفاح  ،   مطرقه   ،   بتيته   ،   لهانه   ،   شباك   ،   باص   ،   سمكه " ، " ذباب " ، " تلفاز " ، " حاسوب " ، " انترنيت " ، " ساحه " ، " جسر " } ؛
name = KlamSpeed ​​[ math.random ( # KlamSpeed)]
قاعدة البيانات: مجموعة (bot_id ..   Klam: Speed   .. msg. chat_id_ ، name)
name =  string.gsub (name،   سحور   ،   س ر و ح   )
name =  string.gsub (name،   سياره   ،   ه ر س ي ا   )
name =  string.gsub (name،   استقبال   ،   ل ب ا ت ق س ا   )
name =  string.gsub (name،   قنفه   ،   ه ق ن ف   )
name =  string.gsub (name،   ايفون   ،   و ن ف ا   )
name =  string.gsub (name،   بزونه   ،   ز و ه ن   )
name =  string.gsub (name،   مطبخ   ،   خ ب ط م   )
name =  string.gsub (name،   كرستيانو   ،   س ت ا ن و ك ر ي   )
name =  string.gsub (name،   دجاجه   ،   ج ج ا د ه   )
name =  string.gsub (name،   مدرسه   ،   ه م د ر س   )
name =  string.gsub (name،   الوان   ،   ن ا و ا ل   )
name =  string.gsub (name،   غرفه   ،   غ ه ر ف   )
name =  string.gsub (name،   ثلاجه   ،   ج ه ت ل ا   )
name =  string.gsub (name،   كهوه   ،   ه ك ه و   )
name =  string.gsub (name،   سفينه   ،   ه ن ف ي س   )
name =  string.gsub (name،   العراق   ،   ق ع ا ل ر ا   )
name =  string.gsub (name،   محطه   ،   ه ط م ح   )
name =  string.gsub (name،   طياره   ،   ر ا ط ي ه   )
name =  string.gsub (name،   رادار   ،   ر ا ر ا د   )
name =  string.gsub (name،   منزل   ،   ن ز م ل   )
name =  string.gsub (name،   مستشفى   ،   ى ش س ف ت م   )
name =  string.gsub (name،   كهرباء   ،   ر ب ك ه ا ء   )
name =  string.gsub (name،   تفاحه   ،   ح ه ا ت ف   )
name =  string.gsub (name،   اخطبوط   ،   ط ب و ا خ ط   )
name =  string.gsub (name،   سلمون   ،   ن م و ل س   )
name =  string.gsub (name،   فرنسا   ،   ن ف ر س ا   )
name =  string.gsub (name،   برتقاله   ،   ر ت ق ب ا ه ل   )
name =  string.gsub (name،   تفاح   ،   ح ف ا ت   )
name =  string.gsub (name،   مطرقه   ،   ه ط م ر ق   )
name =  string.gsub (name،   بتيته   ،   ب ت ت ي ه   )
name =  string.gsub (name،   لهانه   ،   ه ن ل ه ل   )
name =  string.gsub (name،   شباك   ،   ب ش ا ك   )
name =  string.gsub (name،   باص   ،   ص ا ب   )
name =  string.gsub (name، " سمكه " ، " ك س م ه " )
name =  string.gsub (name،   ذباب   ،   ب ا ب ذ   )
name =  string.gsub (name،   تلفاز   ،   ت ف ل ز ا   )
name =  string.gsub (name،   حاسوب   ،   س ا ح و ب   )
name =  string.gsub (name،   انترنيت   ،   ا ت ن ر ن ي ت   )
name =  string.gsub (name،   ساحه   ،   ح ا ه س   )
name =  string.gsub (name،   جسر   ،   ر ج س   )
أرسل (msg. chat_id_ ، msg. id_ ،   ✯اسرع واحد يرتبها »{   .. name ..   }   )
عودة  كاذبة
نهاية
نهاية
- ------------------------------------------------ ----------------------
إذا نص ==      .. (قاعدة بيانات: الحصول على (bot_id .. " Klam: سرعة " .. . جي اس chat_id_ ) أو      ) ..      و  ليس قاعدة البيانات: الحصول على (bot_id .. " السرعة: آر " . . جي اس. chat_id_ ) ثم
إذا  لم تكن قاعدة البيانات: احصل على (bot_id ..   Speed: Tr   .. msg. chat_id_ ) إذن 
send (msg. chat_id_ ، msg. id_ ،   ✯الف مبروك لقد فزت \ n ✯للعب مره اخره ارسل »{الاسرع، ترتيب}   )
قاعدة البيانات: incrby (bot_id ..   NUM: GAMES   .. msg. chat_id_ .. msg. sender_user_id_ ، 1 )  
نهاية
قاعدة البيانات: مجموعة (bot_id ..   Speed: Tr   .. msg. chat_id_ ، true )
نهاية 

إذا كان النص ==    الحزوره    أو النص ==    حزوره    إذن
إذا كان  AddChannel (msg. sender_user_id_ ) ==  false  إذًا
local textchuser = قاعدة البيانات: get (bot_id ..   text: ch: user   )
إذا textchuser ثم
إرسال (msg. chat_id_ ، msg. id_ ،   [   .. textchuser ..   ]   )
آخر
send (msg. chat_id_ ، msg. id_ ،    تستطيع استخدام البوت \ n   ✯يرجى بخدمة بالقناه اولا \ n   ✯اشترك هنا [   .. قاعدة بيانات: get (bot_id ..   add: ch: username   ) ..   ]   )
نهاية
عودة  كاذبة
نهاية
إذا كانت قاعدة البيانات: احصل على (bot_id ..   Lock: Games   .. msg. chat_id_ ) ثم
قاعدة البيانات: del (bot_id ..   Set: Hzora   .. msg. chat_id_ )
Hzora = {   الجرس   ،   عقرب الساعه   ،   السمك   ،   المطر   ،   5   ،   الكتاب   ،   البسمار   ،   7   ،   الكعبه   ،   بيت الشعر   ،   لهانه   ،   انا   ،   امي  ،  الابره   ،   الساعه   ،   22   ،   غلط   ،   كم الساعه   ،   البيتنجان   ،   البيض   ،   المرايه   ،   الضوء   ،   الهواء   ،   الضل   ،   العمر   ،   القلم   ،   المشط "،  الحفره   ،   البحر   ،   الثلج   ،   الاسفنج   ،   الصوت   ،   بلم   }؛
الاسم = هزورا [ math.random ( # Hzora)]
قاعدة البيانات: مجموعة (bot_id ..   Klam: Hzor   .. msg. chat_id_ ، name)
name =  string.gsub (name،   الجرس   ،   شيئ إذا لمسته صرخ ما هوه؟   )
name =  string.gsub (name،   عقرب الساعه   ،   اخوان لا يستطيع تمضيه اكثر من دقيقه معا فما هما؟   )
name =  string.gsub (name، " السمك " ، " ما هو الحيوان الذي لم يصعد سفينة نوح عليه السلام؟ " )
name =  string.gsub (name،   المطر   ،   شيئ يسقط على رأسك من الاعلى ولا يجرحك فما هو؟   )
name =  string.gsub (name،   5   ،   ما العدد الذي اذا ضربته بنفسه واضفت عليه 5 يصبح ثلاثين   )
name =  string.gsub (name،   الكتاب   ،   ما الشيئ الذي له أوراق وليس له جذور؟   )
name =  string.gsub (name،   البسمار   ،   ما هو الشيئ الذي لا يمشي الا بالضرب؟   )
name =  string.gsub (name،   7   ،   عائله مؤلفه من 6 بنات واخ لكل منهن .فكم عدد افراد العائله   )
name =  string.gsub (name،   الكعبه   ،   ما هو الشيئ الموجود وسط مكة؟   )
name =  string.gsub (name،   بيت الشعر   ،   ما هو البيت الذي ليس فيه ابواب ولا نوافذ؟   )
name =  string.gsub (name،   لهانه   ،   وحده حلوه ومغروره تلبس مية تنوره .من هيه؟   )
name =  string.gsub (name،   انا   ،   ابن امك وابن ابيك وليس باختك ولا باخيك فمن يكون؟   )
name =  string.gsub (name،   امي   ،   اخت خالك وليست خالتك من تكون؟   )
name =  string.gsub (name،   الابره   ،   ما هو الشيئ الذي كلما خطاوه خطاوه فقده من ذيله؟   )
name =  string.gsub (name،   الساعه   ،   ما هو الشيئ الذي يقول الصدق ولكنه إذا جاع كذب؟   )
name =  string.gsub (name،   22   ،   كم مره ينطبق عقربا الساعه على بعضهما في اليوم الواحد   )
name =  string.gsub (name،   غلط   ،   ما هي الكلمه الوحيده التي تلفض غلط دائما؟   )
name =  string.gsub (name،   كم الساعه   ،   ما هو السؤال الذي يختلف اجابته دائما؟   )
name =  string.gsub (name،   البيتنجان   ،   جسم اسود وقلب ابيض ورأس اخظر فما هو؟   )
name =  string.gsub (name،   البيض   ،   ماهو الشيئ الذي اسمه على لونه؟   )
name =  string.gsub (name،   المرايه   ،   ارى كل شيئ من دون عيون من اكون؟   )
name =  string.gsub (name،   الضوء   ،   ما هو الشيئ الذي يخترق الزجاج ولا يكسره؟   )
name =  string.gsub (name،   الهواء   ،   ما هو الشيئ الذي يسير امامك ولا تراه؟   )
name =  string.gsub (name،   الضل   ،   ما هو الشيئ الذي يلاحقك اينما تذهب؟   )
name =  string.gsub (name،   العمر   ،   ما هو الشيء الذي كلما طال قصر؟   )
name =  string.gsub (name،   القلم   ،   ما هو الشيئ الذي يكتب ولا يقرأ؟   )
name =  string.gsub (name،   المشط   ،   له أسنان ولا يعض ما هو؟   )
name =  string.gsub (name، " الحفر " ، " ما هو الشيئ عند أخذنا منه وكبر؟ " )
name =  string.gsub (name،   البحر   ،   ما هو الشيئ الذي يرفع اثقال ولا يقدر يرفع مسمار؟   )
name =  string.gsub (name،   الثلج   ،   انا ابن الماء فان تركوني في الماء مت فمن انا؟   )
name =  string.gsub (name،   الاسفنج   ،   كلي ثقوب ومع ذالك احفض الماء فمن اكون؟   )
name =  string.gsub (name،   الصوت   ،   اسير بلا رجلين ولا ادخل الا بالاذنين فمن انا؟   )
name =  string.gsub (name،   بلم   ،   حامل ومحمول نصف ناشف ونصف مبلول فمن اكون؟   )
أرسل (msg. chat_id_ ، msg. id_ ،   ✯اسرع واحد يحل الحزوره ↓ \ n {   .. name ..   }   )
عودة  كاذبة
نهاية
نهاية
- ------------------------------------------------ ----------------------
إذا نص ==      .. (قاعدة بيانات: الحصول على (bot_id .. " Klam: Hzor " .. . جي اس chat_id_ ) أو      ) ..      و  ليس قاعدة البيانات: الحصول على (bot_id ..   مجموعة: Hzora " . . جي اس. chat_id_ ) ثم
إذا  لم تكن قاعدة البيانات: احصل على (bot_id ..   Set: Hzora   .. msg. chat_id_ ) ثم 
send (msg. chat_id_ ، msg. id_ ،   ✯الف مبروك لقد فزت \ n ✯للعب مره اخره ارسل »{حزوره}   )
قاعدة البيانات: incrby (bot_id ..   NUM: GAMES   .. msg. chat_id_ .. msg. sender_user_id_ ، 1 )  
نهاية
قاعدة البيانات: set (bot_id ..   Set: Hzora   .. msg. chat_id_ ، true )
نهاية 

إذا كان النص ==    المعاني    أو text ==    معاني    إذن
إذا كان  AddChannel (msg. sender_user_id_ ) ==  false  إذًا
local textchuser = قاعدة البيانات: get (bot_id ..   text: ch: user   )
إذا textchuser ثم
إرسال (msg. chat_id_ ، msg. id_ ،   [   .. textchuser ..   ]   )
آخر
send (msg. chat_id_ ، msg. id_ ،    تستطيع استخدام البوت \ n   ✯يرجى بخدمة بالقناه اولا \ n   ✯اشترك هنا [   .. قاعدة بيانات: get (bot_id ..   add: ch: username   ) ..   ]   )
نهاية
عودة  كاذبة
نهاية
إذا كانت قاعدة البيانات: احصل على (bot_id ..   Lock: Games   .. msg. chat_id_ ) ثم
قاعدة البيانات: del (bot_id ..   Set: Maany   .. msg. chat_id_ )
Maany_Rand = {   قرد   ،   دجاجه   ،   بطريق   ،   ضفدع   ،   بومه   ،   نحله   ،   ديك   ،   جمل   ،   بقره   ،   دولفين   ،   تمساح   ،   قرش   ، " نمر " ،  اخطبوط  ،   سمكه   ،   خفاش   ،   اسد   ،   فأر   ،   ذئب   ،   فراشه   ،   عقرب   ،   زرافه   ،   قنفذ   ،   تفاحه   ،   باذنجان   }
الاسم = Maany_Rand [ math.random ( # Maany_Rand)]
قاعدة البيانات: set (bot_id ..   Maany   .. msg. chat_id_ ، name)
name =  string.gsub (name،   قرد   ،   🐒   )
name =  string.gsub (name،   دجاجه   ،   🐔   )
name =  string.gsub (name،   بطريق   ،   🐧   )
الاسم =  string.gsub (الاسم ،   ضفدع   ،   🐸   )
الاسم =  string.gsub (الاسم ، " بومه " ، " 🦉 " )
الاسم =  string.gsub (name،   نحله   ،   🐝   )
name =  string.gsub (name،   ديك   ،   🐓   )
name =  string.gsub (name،   جمل   ،   🐫   )
name =  string.gsub (name،   بقره   ،   🐄   )
name =  string.gsub (name،   دولفين   ،   ؟؟   )
name =  string.gsub (name،   تمساح   ،   🐊   )
name =  string.gsub (name،   قرش   ،   🦈   )
name =  string.gsub (name،   نمر   ،   🐅   )
name =  string.gsub (name،   اخطبوط   ،   🐙   )
الاسم =  string.gsub (الاسم ، " سمكه " ، " 🐟 " )
name =  string.gsub (name،   خفاش   ،   🦇   )
name =  string.gsub (name،   اسد   ،   🦁   )
name =  string.gsub (الاسم ، " فأر " ، " 🐭 " )
name =  string.gsub (name،   ذئب   ،   🐺   )
name =  string.gsub (name،   فراشه   ،   🦋   )
name =  string.gsub (name،   عقرب   ،   🦂   )
name =  string.gsub (name،   زرافه   ،   🦒   )
name =  string.gsub (name،   قنفذ   ،   🦔   )
name =  string.gsub (name،   تفاحه   ،   🍎   )
الاسم =  string.gsub (الاسم ، " باذنجان " ، " 🍆 " )
send (msg. chat_id_ ، msg. id_ ،   ✯اسرع واحد يدز معنى السمايل »{   .. name ..   }   )
عودة  كاذبة
نهاية
نهاية
- ------------------------------------------------ ----------------------
إذا نص ==      .. (قاعدة بيانات: الحصول على (bot_id .. " Maany " .. جي اس. chat_id_ ) أو      ) ..      و  ليس قاعدة البيانات: الحصول على (bot_id ..   مجموعة: Maany " .. جي اس . chat_id_ ) ثم
إذا  لم تكن قاعدة البيانات: احصل على (bot_id ..   Set: Maany   .. msg. chat_id_ ) ثم 
send (msg. chat_id_ ، msg. id_ ،   ✯ الف مبروك لقد فزت \ n ✯للعب مره اخره ارسل »{معاني}   )
قاعدة البيانات: incrby (bot_id ..   NUM: GAMES   .. msg. chat_id_ .. msg. sender_user_id_ ، 1 )  
نهاية
قاعدة البيانات: set (bot_id ..   Set: Maany   .. msg. chat_id_ ، true )
نهاية 
إذا كان النص ==    العكس    أو text ==    عكس    ثم
إذا كان  AddChannel (msg. sender_user_id_ ) ==  false  إذًا
local textchuser = قاعدة البيانات: get (bot_id ..   text: ch: user   )
إذا textchuser ثم
إرسال (msg. chat_id_ ، msg. id_ ،   [   .. textchuser ..   ]   )
آخر
إرسال (msg.chat_id_ ، msg. id_ ،    تستطيع استخدام البوت \ n   ✯يرجى بخدمة بالقناه اولا \ n   ✯اشترك هنا [   .. قاعدة بيانات: get (bot_id ..   add: ch: username   ) ..   ]   )
نهاية
عودة  كاذبة
نهاية
إذا كانت قاعدة البيانات: احصل على (bot_id ..   Lock: Games   .. msg. chat_id_ ) ثم
قاعدة البيانات: del (bot_id ..   Set: Aks   .. msg. chat_id_ )
katu = {   باي   ،   فهمت   ،   موزين " ، " اسمعك " ، " احبك " ، " موحلو   ،   نضيف   ،   حاره " ، ، " ضعيف " ، " شريف " ، " شجاع " ، " رحت   ،   عدل  ،   نشيط   ناصي   ،   جوه   ،   سريع   ،   ونسه   ، " طويل " ،" سمين "" ،   شبعان   ،   موعطشان   ،   خوش ولد   ،   اني   ،   هادئ   }
الاسم = كاتو [ math.random ( # كاتو )]
قاعدة البيانات: مجموعة (bot_id ..   Set: Aks: Game   .. msg. chat_id_ ، name)
name =  string.gsub (name،   باي   ،   هلو   )
name =  string.gsub (name،   فهمت   ،   مافهمت   )
name =  string.gsub (الاسم ، " موزين " ، " زين " )
name =  string.gsub (name،   اسمعك   ،   ماسمعك   )
name =  string.gsub (name،   احبك   ،   ماحبك   )
name =  string.gsub (name،   موحلو   ،   حلو   )
name =  string.gsub (name،   نضيف   ،   وصخ   )
اسم  =  string.gsub (name،   حاره   ،   بارده   )
اسم  =  string.gsub (name،   ناصي   ،   عالي   )
اسم  =  string.gsub (الاسم، " جوه " ، " فوك " )
اسم  =  string.gsub (الاسم ، " سريع " ، " بطيء " )
name =  string.gsub (name،   ونسه   ،   ضوجه   )
الاسم =  string.gsub (الاسم ، " طويل " ، " قزم " )
name =  string.gsub (الاسم ، " سمين " ، " ضعيف " )
name =  string.gsub (name،   ضعيف   ،   قوي   )
الاسم =  string.gsub (name، " شريف " ، " كواد " )
الاسم =  string.gsub (name،   شجاع   ،   جبان   )
الاسم =  string.gsub (name،   رحت   ،   اجيت   )
الاسم =  string.gsub (name،   عدل   ،   ميت   )
الاسم =  string.gsub (name،   نشيط   ،   كسول   )
الاسم =  string.gsub (name،   شبعان   ،   جوعان   )
الاسم =  string.gsub (name، " موعطشان " ، " عطشان " )
الاسم =  string.gsub (name،   خوش ولد   ،   موخوش ولد   )
name =  string.gsub (name،   اني   ،   مطي   )
name =  string.gsub (name،   هادئ   ،   عصبي   )
send (msg. chat_id_ ، msg. id_ ،   ✯اسرع واحد يدز العكس »{   .. name ..   }   )
عودة  كاذبة
نهاية
نهاية
- ------------------------------------------------ ----------------------
إذا نص ==      .. (قاعدة بيانات: الحصول على (bot_id ..   مجموعة: AKS: لعبة " .. جي اس. chat_id_ ) أو      ) ..      و  ليس قاعدة البيانات: الحصول على (bot_id ..   مجموعة: AKS " .. msg. chat_id_ ) ثم
إذا  لم تكن قاعدة البيانات: احصل على (bot_id ..   Set: Aks   .. msg. chat_id_ ) ثم 
إرسال (msg. chat_id_ ، msg. id_ ،   ✯الف مبروك لقد فزت \ n ✯للعب مره اخره ارسل »{العكس}   )
قاعدة البيانات: incrby (bot_id ..   NUM: GAMES   .. msg. chat_id_ .. msg. sender_user_id_ ، 1 )  
نهاية
قاعدة البيانات: مجموعة (bot_id ..   Set: Aks   .. msg. chat_id_ ، true )
نهاية 

إذا كانت قاعدة البيانات: احصل على (bot_id .. " GAME: TKMEN "  .. msg. chat_id_  ..  " "  .. msg. sender_user_id_ ) ثم  
إذا النص و النص: مباراة ( " ^ (٪ د +) $ " ) ثم
محلي NUM = نص: تطابق ( " ^ (٪ d +) $ " )
إذا كان  طن (NUM) >  20  ثم
أرسل (msg. chat_id_ ، msg. id_ ، " ✯عذرآ لا يمكنك تخمين عدد أكبر من {20} خمن رقم ما بين ال {1 و 20} \ n " )
عودة  خاطئة   نهاية 
GETNUM المحلي = قاعدة البيانات: get (bot_id .. " GAMES: NUM " .. msg. chat_id_ )
إذا  tonumber (NUM) ==  tonumber (GETNUM) ثم
قاعدة البيانات: del (bot_id ..   SADD: NUM   .. msg. chat_id_ .. msg. sender_user_id_ )
قاعدة البيانات: del (bot_id .. " GAME: TKMEN "  .. msg. chat_id_  ..  " "  .. msg. sender_user_id_ )   
قاعدة البيانات: incrby (bot_id ..   NUM: GAMES   .. msg. chat_id_ .. msg. sender_user_id_ ، 5 )  
send (msg. chat_id_ ، msg. id_ ،   ✯مبروك فزت ويانه وخمنت الرقم الصحيح \ n ✯تم اضافة {5} من النقاط \ n   )
ELSEIF  tonumber (NUM) ~ = tonumber (GETNUM) إذن
قاعدة البيانات: incrby (bot_id ..   SADD: NUM   .. msg. chat_id_ .. msg. sender_user_id_ ، 1 )
if  tonumber (قاعدة البيانات: get (bot_id ..   SADD: NUM   .. msg. chat_id_ .. msg. sender_user_id_ )) > =  3  ثم
قاعدة البيانات: del (bot_id ..   SADD: NUM   .. msg. chat_id_ .. msg. sender_user_id_ )
قاعدة البيانات: del (bot_id .. " GAME: TKMEN "  .. msg. chat_id_  ..  " "  .. msg. sender_user_id_ )   
send (msg. chat_id_ ، msg. id_ ،   ✯اوبس لقد خسرت في اللعبه \ n ✯حظآ اوفر في المره القادمه \ n ✯كان الرقم الذي تم تخمينه {   .. GETNUM ..   }   )
آخر
send (msg. chat_id_ ، msg. id_ ،   ✯اوبس تخمينك غلط \ n ✯ارسل رقم تخمنه مره اخرى   )
نهاية
نهاية
نهاية
نهاية
إذا كان النص ==    خمن    أو text ==    التخمين    إذن   
إذا كان  AddChannel (msg. sender_user_id_ ) ==  false  إذًا
local textchuser = قاعدة البيانات: get (bot_id ..   text: ch: user   )
إذا textchuser ثم
إرسال (msg. chat_id_ ، msg. id_ ،   [   .. textchuser ..   ]   )
آخر
send (msg. chat_id_ ، msg. id_ ،    تستطيع استخدام البوت \ n   ✯يرجى بخدمة بالقناه اولا \ n   ✯اشترك هنا [   .. قاعدة بيانات: get (bot_id ..   add: ch: username   ) ..   ]   )
نهاية
إرجاع كاذبة
نهاية
إذا كانت قاعدة البيانات: احصل على (bot_id .. " قفل: الألعاب " .. رسالة. chat_id_ ) ثم
Num =  math.random ( 1 ، 20 )
قاعدة البيانات: مجموعة (bot_id .. " GAMES: NUM " .. msg. chat_id_ ، Num)
أرسل (msg. chat_id_ ، msg. id_ ،   \ n ✯اهلا بك عزيزي في لعبة التخمين: \ n ٴ━━━━━━━━━━ \ n   ..   ✯ملاحظه لديك {3} محاولات فقط فكر قبل ارسال تخمينك \ n \ n   ..   ✯سيتم تخمين عدد ما بين ال {1 و 20} اذا كنت تعتقد انك تستطيع الفوز جرب واللعب الان؟   )
قاعدة البيانات: setex (bot_id .. " GAME: TKMEN "  .. msg. chat_id_  ..  " "  .. msg. sender_user_id_ ، 100 ، true )  
عودة  كاذبة  
نهاية
نهاية

إذا كانت قاعدة البيانات: احصل على (bot_id .. " SET: GAME "  .. msg. chat_id_  ..  " "  .. msg. sender_user_id_ ) ثم  
إذا النص و النص: مباراة ( " ^ (٪ د +) $ " ) ثم
محلي NUM = نص: تطابق ( " ^ (٪ d +) $ " )
إذا كان  طن (NUM) >  6  ثم
send (msg. chat_id_ ، msg. id_ ، " ✯عذرا لا يوجد سواء {6} اختيارات فقط ارسل اختيارك مره اخرى \ n " )
عودة  خاطئة   نهاية 
GETNUM المحلي = قاعدة البيانات: get (bot_id .. " Games: Bat " .. msg. chat_id_ )
إذا  tonumber (NUM) ==  tonumber (GETNUM) ثم
قاعدة البيانات: del (bot_id .. " SET: GAME "  .. msg. chat_id_  ..  " "  .. msg. sender_user_id_ )   
send (msg. chat_id_ ، msg. id_ ،   ✯مبروك فزت وطلعت المحيبس بل ايد رقم {   .. NUM ..   } \ n ✯لقد حصلت على {3} نقاط يمكنك تطويرهن برسائل   )
قاعدة البيانات: incrby (bot_id ..   NUM: GAMES   .. msg. chat_id_ .. msg. sender_user_id_ ، 3 )  
elseif  tonumber (NUM) ~ =  tonumber (GETNUM) إذن
قاعدة البيانات: del (bot_id .. " SET: GAME "  .. msg. chat_id_  ..  " "  .. msg. sender_user_id_ )   
send (msg. chat_id_ ، msg. id_ ،   ✯للاسف لقد خسرت \ n ✯المحيبس بل ايد رقم {   .. الحصول على ..   } \ n ✯حاول مره اخرى على المحيبس   )
نهاية
نهاية
نهاية

إذا كان النص ==    محيبس    أو text ==    البات    إذن
إذا كان  AddChannel (msg. sender_user_id_ ) ==  false  إذًا
local textchuser = قاعدة البيانات: get (bot_id ..   text: ch: user   )
إذا textchuser ثم
إرسال (msg. chat_id_ ، msg. id_ ،   [   .. textchuser ..   ]   )
آخر
send (msg. chat_id_ ، msg. id_ ،    تستطيع استخدام البوت \ n   ✯يرجى بخدمة بالقناه اولا \ n   ✯اشترك هنا [   .. قاعدة بيانات: get (bot_id ..   add: ch: username   ) ..   ]   )
نهاية
عودة  كاذبة
نهاية
إذا كانت قاعدة البيانات: احصل على (bot_id ..   Lock: Games   .. msg. chat_id_ ) ثم   
العدد =  math.random ( 1 ، 6 )
قاعدة البيانات: مجموعة (bot_id .. " الألعاب: بات " .. msg. chat_id_ ، Num)
اختبار =  [[
* ➀ ➁ ➂ ➃ ➄ ➅
↓ ↓ ↓ ↓ ↓ ↓   
👊 ‹•› ‹•› ›‹ • ›‹ • ›👊
 خراجاختر لأستخراج المحيبس الايد التي تحمل المحيبس 
 ✯الفائز يحصل على {3} من النقاط *
]]
إرسال (msg. chat_id_ ، msg. id_ ، TEST)
قاعدة البيانات: setex (bot_id .. " SET: GAME "  .. msg. chat_id_  ..  " "  .. msg. sender_user_id_ ، 100 ،true)  
عودة  كاذبة  
نهاية
نهاية

- ------------------------------------------------ ----------------------
if text ==  المختلف  or text ==  مختلف  then
if AddChannel(msg.sender_user_id_) == false then
local textchuser = database:get(bot_id.. text:ch:user )
if textchuser then
send(msg.chat_id_, msg.id_, [ ..textchuser.. ] )
else
send(msg.chat_id_, msg.id_,  ✯لا تستطيع استخدام البوت \n  ✯يرجى الاشتراك بالقناه اولا \n  ✯اشترك هنا [ ..database:get(bot_id.. add:ch:username ).. ] )
end
return false
end
if database:get(bot_id.. Lock:Games ..msg.chat_id_) then
mktlf = { 😸 , ☠ , 🐼 , 🐇 , ?? , 🌚 , ⭐️ , ✨ , ⛈ , 🌥 , ⛄️ , 👨‍🔬 , 👨‍💻 , 👨‍🔧 , 👩‍🍳 , 🧚‍♀ , ??‍♂ , ??‍♂ , 🙍‍♂ , 🧖‍♂ , 👬 , 👨‍👨‍👧 , 🕒 , 🕤 , ⌛️ , 📅 ,};
name = mktlf[math.random(#mktlf)]
database:del(bot_id.. Set:Moktlf:Bot ..msg.chat_id_)
database:set(bot_id.. :Set:Moktlf ..msg.chat_id_,name)
name = string.gsub(name, 😸 , 😹😹😹😹😹😹😹😹😸😹😹😹😹 )
name = string.gsub(name, ☠ , 💀💀💀💀💀💀💀☠💀💀💀💀💀 )
name = string.gsub(name, 🐼 , 👻👻👻🐼👻👻👻👻👻👻👻 )
name = string.gsub(name, 🐇 , 🕊🕊🕊🕊🕊🐇🕊🕊🕊🕊 )
name = string.gsub(name, 🌑 , 🌚🌚🌚🌚🌚🌑🌚🌚🌚 )
name = string.gsub(name, 🌚 , 🌑🌑🌑🌑🌑🌚🌑🌑🌑 )
name = string.gsub(name, ⭐️ , 🌟🌟🌟🌟🌟🌟🌟🌟⭐️🌟🌟🌟 )
name = string.gsub(name, ✨ , 💫💫💫💫💫✨💫💫💫💫 )
name = string.gsub(name, ⛈ , 🌨🌨🌨🌨🌨⛈🌨🌨🌨🌨 )
name = string.gsub(name, 🌥 , ⛅️⛅️⛅️⛅️⛅️⛅️🌥⛅️⛅️⛅️⛅️ )
name =  string.gsub (name،   ⛄️   ،   ☃☃☃☃☃☃⛄️☃☃☃☃   )
name =  string.gsub (name،   👨‍🔬   ،   👩‍🔬👩‍🔬👩‍🔬👩‍🔬👩‍🔬👩‍🔬👩‍🔬👩‍🔬👨‍🔬👩‍🔬👩‍🔬👩‍ 🔬 " )
name =  string.gsub (name،   👨‍💻   ،   👩‍💻👩‍💻👩‍‍💻👩‍‍💻👩‍💻👨‍💻👩‍💻👩‍💻👩‍💻   )
name =  string.gsub (name،   👨‍🔧   ،   👩‍🔧👩‍🔧👩‍🔧👩‍🔧👩‍🔧👩‍🔧👨‍🔧👩‍🔧   )
الاسم =  string.gsub (الاسم ،   👩‍🍳   ،   👨‍🍳👨‍🍳👨‍🍳👨‍🍳👨‍🍳👩‍🍳👨‍🍳👨‍🍳👨‍🍳   )
name =  string.gsub (name،   🧚‍♀   ،   🧚‍♂🧚‍♂🧚‍♂🧚‍♂🧚‍♀🧚‍♂🧚‍♂   )
name =  string.gsub (name،   🧜‍♂   ،   🧜‍♀🧜‍♀🧜‍♀🧜‍♀🧜‍♀🧚‍♂🧜‍♀🧜‍♀🧜‍♀   )
name =  string.gsub (name،   🧝‍♂   ،   🧝‍♀🧝‍♀🧝‍♀🧝‍♀🧝‍♀🧝‍♂🧝‍♀🧝‍♀🧝‍♀   )
name =  string.gsub (name،   🙍‍♂️   ،   🙎‍♂️🙎‍♂️🙎‍♂️🙎‍♂️🙎‍♂️🙍‍♂️🙎‍♂️🙎‍♂️🙎‍♂️   )
name =  string.gsub (name،   🧖‍♂️   ،   🧖‍♀️🧖‍♀️🧖‍♀️🧖‍♀️🧖‍♀️🧖‍♂️🧖‍♀️🧖‍♀️🧖‍♀️🧖‍♀️   )
name =  string.gsub (name،   👬   ،   👭👭👭👭👭👬👭👭👭   )
name =  string.gsub (name،   👨‍👨‍👧   ،   👨‍👨‍👦👨‍👨‍👦👨‍👨‍👦👨‍👨‍👦👨‍👨‍👧👨‍👨‍👦👨‍👨 ‍👦   )
name =  string.gsub (name،   🕒   ،   🕒🕒🕒🕒🕒🕒🕓🕒🕒🕒   )
name =  string.gsub (name،   🕤   ،   🕥🕥🕥🕥🕥🕤🕥🕥🕥   )
name =  string.gsub (name،   ⌛️   ،   ⏳⏳⏳⏳⏳⏳⌛️⏳⏳   )
n


