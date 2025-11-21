# Go Development Workflow in Emacs

## 🚀 **The Go Menu - Your Command Center**
Press `<leader>gm` in any Go buffer to open the **transient menu** - this is your main interface:

```
Go: Build • Test • Run • Debug
──────────────────────────────
Run/Build    Tests              Debug
b  Build     t  Test at point   d  Debug: choose template
r  Run       f  Test file       .  Toggle breakpoint
R  Re-run    p  Test project    ,  Continue
             k  Benchmarks
             c  Coverage toggle

g  Open *compilation*  q  Quit menu
```

## 🔥 **Daily Workflow Shortcuts**

### **Quick Actions (use these constantly):**
- `<leader>gr` - **Run your Go project** (`go run .`)
- `<leader>gb` - **Build** (`go build ./...`)
- `<F5>` - **Re-run last command** (build/test/run)
- `<leader>gp` - **Test entire project** (uses `gotestsum` if available)

### **Development Flow:**
1. **Code** → `<leader>gr` (run to see it work)
2. **Test** → `<leader>gp` (run all tests)
3. **Build** → `<leader>gb` (check for build errors)
4. **Fix** → `<F5>` (re-run whatever failed)

## 🎯 **Best Features of This Setup**

### **1. Smart Compilation Window**
- **Auto-appears at bottom** when you run commands
- **Auto-scrolls** to errors
- **ANSI colors** for pretty output
- **Quick access** via `g` in the Go menu

### **2. LSP Integration (Automatic)**
- **Format + organize imports** on every save
- **gofumpt** formatting (stricter than gofmt)
- **staticcheck** linting built-in
- Normal LSP shortcuts still work: `gd` (definition), `gr` (references)

### **3. Testing Made Easy**
- `<leader>gt` - **Test function at cursor**
- `<leader>gf` - **Test current file**
- `<leader>gp` - **Test whole project**
- Uses **gotestsum** if installed (prettier output)

### **4. Debugging with Delve**
- `<leader>gd` - **Start debugger** (choose template)
- `<leader>g.` - **Toggle breakpoint**
- Pre-configured templates for main and tests

### **5. Coverage Visualization**
- `<leader>gc` - **Toggle coverage overlay**
- Shows which lines are covered by tests

## 💡 **Pro Tips**

### **Error Navigation:**
When compilation fails, the **compilation buffer** opens automatically. Use:
- `C-x `` (backtick) - Jump to next error
- Click errors to jump to source

### **Project Structure:**
- Works great with **go.mod** projects
- Auto-detects project root (uses projectile if you have it)
- Runs commands from project root automatically

### **Performance:**
- **Tree-sitter** gives you fast, accurate syntax highlighting
- **LSP completion** via Corfu (no more company-mode conflicts)
- **Background compilation** doesn't block Emacs

## 🎨 **Visual Cues**
- **Compilation window** appears at bottom (33% height)
- **Breadcrumbs** in header line show current function/struct
- **Semantic highlighting** from LSP
- **Error squiggles** and sideline diagnostics

## 🚦 **Common Workflows**

### **New Feature Development:**
```
1. <leader>gr  (run to see current state)
2. Write code...
3. <leader>gt  (test the function you're working on)
4. <leader>gp  (run all tests when done)
5. <leader>gb  (final build check)
```

### **Debugging Session:**
```
1. <leader>g.  (set breakpoint)
2. <leader>gd  (start debugger, choose "Launch main")
3. <leader>g,  (continue execution)
4. Use DAP UI to inspect variables
```

### **Test-Driven Development:**
```
1. <leader>gt  (run failing test)
2. Write code...
3. <F5>       (re-run same test)
4. Repeat until green
```

## 🖥️ **Shell Buffer Management**

### **Quick Shell Access:**
- `<leader><leader>t` - **Toggle last used shell buffer** ⭐
- `<leader>t0-9` - Toggle specific shell buffer (remembers)
- `<leader>to0-9` - Toggle specific shell buffer in other window (remembers)

### **Shell Workflow:**
1. **Open any shell**: `<leader>t3` (opens shell buffer 3)
2. **Do some work**, switch to other buffers...
3. **Quick return**: `<leader><leader>t` (instantly back to shell 3)
4. **Switch shells**: `<leader>t7` (opens shell 7, now that's remembered)
5. **Quick return**: `<leader><leader>t` (now goes to shell 7)

---

The **transient menu** (`<leader>gm`) is your best friend - it shows all options and you can press keys without the leader prefix once it's open. The setup is designed for **fast iteration** and **minimal context switching** between Emacs and terminal.