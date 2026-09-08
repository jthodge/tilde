-- Syntax and keymap checks without loading installed plugins or user state.
local root = vim.fn.getcwd() .. "/nvim/.config/nvim"
local files = vim.fn.globpath(root, "**/*.lua", false, true)
for _, path in ipairs(files) do
  assert(loadfile(path), "Lua parse failed: " .. path)
end

local noop = function() end
Snacks = {
  picker = setmetatable({}, { __index = function() return noop end }),
  bufdelete = { delete = noop },
  rename = { rename_file = noop },
  gitbrowse = { open = noop },
}
local bindings = {}
package.preload["which-key"] = function()
  return { add = function(entries) bindings = entries end }
end
vim.g.mapleader = " "
dofile(root .. "/lua/keymaps.lua")
local seen = {}
for _, mapping in ipairs(bindings) do
  assert(not seen[mapping[1]], "Duplicate key: " .. mapping[1])
  seen[mapping[1]] = mapping
end
assert(seen["<leader>E"][2] == "<cmd>Ex<cr>")
assert(seen["<leader>qq"][2] == "<cmd>wqa<cr>")
assert(seen["<leader>q"].group == "Neovim")
assert(seen["<leader>tt"].desc == "Switch Theme")
assert(not seen["<leader>Eq"])
print("PASS: " .. #files .. " Lua files parse; keymaps have no duplicate entries")
