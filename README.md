# gtd-review

### _Jonathan A. Bennett <doulos05@gmail.com>_

This is a suite of tools to guide a user through a thorough weekly review using Taskwarrior to store active tasks.

## License

MIT

## Installation

These instructions assume you have Steel Bank Common Lisp and Quicklisp installed and configured on your system. This should work with any Common Lisp, but has only been tested on SBCL

1. Clone the repository into your local-projects folder (typically `~/quicklisp/local-projects`).
2. Launch `sbcl` and run `(asdf:make "gtd-review")`
3. `mkdir ~/.cl-gtd` and `touch ~/.cl-gtd/projects.txt`
4. `mv ~/quicklisp/local-projects/gtd-review/on-exit-projects-list ~/.task/hooks/`

Now, new projects will get added to your `projects.txt` list every time you create one in Taskwarrior. But this is addition only, projects are never automatically deleted from the `projects.txt` file. The fundamental assumption about projects that this tool makes is that only you can decide whether a project is finished or not, therefore only you can remove it from the projects list.

## Roadmap

### 0.0.1

This is the current release. It only has the on-exit hook for taskwarrior.

### 0.0.2

- Adjust directory structure to support the other parts of the system.
- Project review interface to examine the list in `projects.txt` one by one and delete them if they are no longer current. User will still need to open another shell to add tasks to a project.

### 0.0.3

- Add ability to create tasks for projects directly from the project review interface.

### 0.1.0

- Add ability to create new projects (and associated tasks) from the project review interface).

### 0.1.1

- Weekly review interface to prompt the user through a weekly review following a fixed script.

### 0.1.2

- Add custom weekly review templates.

### 0.2.0

- Weekly Review feature should be complete.
