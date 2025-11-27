
vim.opt.syntax = "enable"

vim.opt.ignorecase = true
vim.opt.mouse = "a"
vim.opt.number = true
vim.opt.autoindent = true

-- http://tedlogan.com/techblog3.html
vim.opt.expandtab = false
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4

-- Haskell wants spaces and I want less indentation:
function haskellike()
	vim.opt_local.expandtab = true
	vim.opt_local.tabstop = 2
	vim.opt_local.shiftwidth = 2
	vim.opt_local.softtabstop = 2
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = "haskell",
  callback = haskellike
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "cabal",
  callback = haskellike
})

-- Nix wants spaces unfortunately:
vim.api.nvim_create_autocmd("FileType", {
  pattern = "nix",
  callback = function()
    vim.opt_local.expandtab = true
  end,
})

-- visualize tabs and trailing spaces:
vim.opt.list = true
vim.opt.listchars = "trail:␣,tab:▸ "

vim.api.nvim_create_autocmd("BufReadPost", {
  callback = function()
    vim.opt.bomb = false
  end,
})

vim.opt.formatoptions = "tcrq"
vim.opt.textwidth = 80

-- Don't enable modelines! They should not be executed
-- on untrusted files.
vim.opt.modeline = false

-- Haskell en Java bestandstypen
-- FIXME: dit heeft qwen3-coder gemaakt op basis van mijn vimrc
--        maar BufRead,BufNewFile werkt niet.
-- vim.api.nvim_create_autocmd("BufRead,BufNewFile", {
--   pattern = "*.ag",
--   callback = function()
--     vim.cmd("set filetype=haskell")
--   end,
-- })
-- 
-- vim.api.nvim_create_autocmd("BufRead,BufNewFile", {
--   pattern = "*.cag",
--   callback = function()
--     vim.cmd("set filetype=haskell")
--   end,
-- })
--
-- java voor il is handiger ivm commentaar (update: ik weet niet eens meer wat
--    il-files zijn...)
-- vim.api.nvim_create_autocmd("BufRead,BufNewFile", {
--   pattern = "*.il",
--   callback = function()
--     vim.cmd("set filetype=java")
--   end,
-- })

-- laat tab-switchen werken in urxvt (gebruik ik niet meer)
-- vim.api.nvim_set_keymap("n", "<ESC>[6^", "<C-PageDown>", { noremap = true, silent = true })
-- vim.api.nvim_set_keymap("n", "<ESC>[5^", "<C-PageUp>", { noremap = true, silent = true })

-- BELOW: een halfbakken ai-plugin.
-- Dat moet nog een plugin worden
-- en het moet veel beter werken.
-- Bijv al: ~/code/bazel gebruiken.

function Bazel(opts)
  local json = vim.fn.json_encode {
    model = "llama3.1",
		prompt = "Er was eens",
		raw = true,
		options = {
      temperature = 0
		}
	}
	local function stdout(err, data)
		vim.schedule(function()
      lines = {data}
	    vim.api.nvim_put(lines, "c", true, true)
    end)
	end
	vim.system({"curl", "--silent", "http://localhost:11434/api/generate", "-d", json},
    {text = true, stdout = stdout })
end

vim.api.nvim_create_user_command("Bazel", Bazel, {})
