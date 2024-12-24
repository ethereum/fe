local M = {}

local ts_repo_url = "https://github.com/fe-lang/tree-sitter-fe.git"
local plugin_runtime_dir = vim.fn.stdpath("data") .. "/nvim-fe-runtime"
local queries_dir = plugin_runtime_dir .. "/queries/fe"
local repo_dir = vim.fn.stdpath("data") .. "/tree-sitter-fe"

-- Ensure a directory exists
local function ensure_dir(dir)
    if vim.fn.isdirectory(dir) == 0 then
        vim.fn.mkdir(dir, "p")
    end
end

-- Prepend the plugin's runtime directory to Neovim's runtime path
local function add_to_runtime()
    if not vim.tbl_contains(vim.opt.runtimepath:get(), plugin_runtime_dir) then
        vim.opt.runtimepath:prepend(plugin_runtime_dir)
    end
end

-- Check if setup is needed
local function needs_setup()
    if vim.fn.isdirectory(repo_dir) == 0 then
        return true
    end
    local query_files = vim.fn.glob(queries_dir .. "/*.scm", false, true)
    return #query_files == 0 -- Check if queries are missing
end

-- Clone or update the tree-sitter-fe repository
local function setup_repository()
    if vim.fn.isdirectory(repo_dir) == 0 then
        vim.fn.system({ "git", "clone", ts_repo_url, repo_dir })
        vim.notify("Cloned tree-sitter-fe repository.", vim.log.levels.INFO)
    else
        vim.fn.system({ "git", "-C", repo_dir, "pull" })
        vim.notify("Updated tree-sitter-fe repository.", vim.log.levels.INFO)
    end
end

-- Set up Tree-sitter queries
local function setup_queries()
    ensure_dir(queries_dir)

    local repo_queries_dir = repo_dir .. "/queries"
    if vim.fn.isdirectory(repo_queries_dir) == 1 then
        for _, query_file in ipairs(vim.fn.glob(repo_queries_dir .. "/*.scm", false, true)) do
            local dest = queries_dir .. "/" .. vim.fn.fnamemodify(query_file, ":t")
            vim.fn.system({ "cp", query_file, dest })
        end
        vim.notify("Fe queries copied to plugin runtime path.", vim.log.levels.INFO)
    else
        vim.notify("No queries directory found in tree-sitter-fe repository.", vim.log.levels.WARN)
    end
end

-- Configure Tree-sitter for Fe
local function setup_treesitter()
    local ok, configs = pcall(require, "nvim-treesitter.configs")
    if not ok then
        vim.notify("nvim-treesitter is not installed. Please install it for Fe syntax highlighting.", vim.log.levels
            .WARN)
        return
    end

    local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
    parser_config.fe = {
        install_info = {
            url = ts_repo_url,
            files = { "src/parser.c", "src/scanner.c" },
            branch = "main",
        },
        filetype = "fe",
    }

    configs.setup({
        ensure_installed = { "fe" },
        highlight = { enable = true },
        indent = { enable = true },
    })
end

-- Set up filetype detection for `.fe` files
local function setup_filetype()
    vim.filetype.add({
        extension = {
            fe = "fe",
        },
    })
end

-- Set up LSP for Fe
local function setup_lsp()
    vim.api.nvim_create_autocmd("FileType", {
        pattern = "fe",
        callback = function()
            local function find_root(patterns)
                local cwd = vim.loop.cwd()
                for _, pattern in ipairs(patterns) do
                    local path = vim.fs.find(pattern, { upward = true, path = cwd })
                    if #path > 0 then
                        return vim.fs.dirname(path[1])
                    end
                end
                return nil
            end

            local root_dir = find_root({ ".git", "fe.toml" })
            if not root_dir then
                vim.notify("Fe LSP: Could not determine root directory.", vim.log.levels.WARN)
                return
            end

            local client_id = vim.lsp.start_client({
                name = "fe",
                cmd = { "fe-language-server" },
                root_dir = root_dir,
                filetypes = { "fe" },
            })

            if client_id then
                vim.lsp.buf_attach_client(0, client_id)
                vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = true })
            end
        end,
    })
end

-- Plugin setup entry point
function M.setup()
    ensure_dir(plugin_runtime_dir)
    add_to_runtime()
    setup_filetype()

    if needs_setup() then
        setup_repository()
        setup_queries()
        setup_treesitter()
        vim.notify("Fe plugin setup completed.", vim.log.levels.INFO)
    else
        setup_treesitter() -- Ensure Tree-sitter is configured
    end

    setup_lsp()
end

return M
