# gtd-review

### _Jonathan A. Bennett <doulos05@gmail.com>_

This is a suite of tools to guide a user through a thorough weekly review using Taskwarrior to store active tasks.

## License

MIT

## Installation

These instructions assume you have Steel Bank Common Lisp and Quicklisp installed and configured on your system. This should work with any Common Lisp, but has only been tested on SBCL

1. Clone the repository into your local-projects folder (typically `~/quicklisp/local-projects`).
2. `make install` to create the gtd-review executable.
3. (Optional) `make hook` to create and install the `taskwarrior` on-exit hook.

## Usage

Run `gtd-review` to trigger a review of your projects. When this is called, the projects in your `projects.txt` file will be displayed one by one and the taskwarrior command `task project:<project> and (status:PENDING or status:WAITING) all` will be called. This gives you a list of pending and waiting tasks for this project. Review this list and evaluate the project for yourself. Do the tasks currently captured in taskwarrior represent everything you need to do with this project? Are any of these tasks stale? Using a _separate_ terminal window, add/modify/complete/delete any tasks that need to be added/modified/completed/deleted in taskwarrior. Finally, mark the project as:

- [a]ctive: This project is still part of my productivity landscape and should remain in my system.
- [c]ompleted: This project is complete and can be removed from my system.
- [d]eleted: This project is no longer relevant and can be removed from my system.

**NOTE** As of version 0.0.2, Complete and Delete have the same behavior! A future release will have different behavior for these two items.

**NOTE 2** This project makes _ZERO_ changes to your taskwarrior tasks. If you mark a project completed or deleted, make the matching edits to your taskwarrior tasks. A future release may add an option to perform this for you.

### The Hook

The hook updates your `projects.txt` file after every time that taskwarrior is run. The update is additive only, that is the hook will never remove a project from your `projects.txt` file. See the section below for why.

## Why This App

This app exists to make reviewing your tasks easier. "But what about `tasksh review`!?" I hear you ask.

### Tasksh review

`tasksh review` is a fantastic tool for looking at every single task in your queue. If you've never used it before, I suggest taking a look, it can be incredibly useful. But it can also be incredibly overwhelming. I'm a teacher, during the semester I have anywhere from 70 to 90 tasks in taskwarrior. At the mid-term and end of term grading periods, I might get up to 120-150. Even assuming I can read each task and make in intelligent decision about what modifications I need to make to that task in 10 seconds per task, that's still 10 to 25 minutes of my weekly review taken up just reviewing my tasks. And at the end of that process, I'm still going to need to go over my projects list and make sure I haven't missed any tasks.

`tasksh review` focuses on tasks because taskwarrior's unit of abstraction is the task. This is completely appropriate and should not be changed. That's because the unit of abstraction when you are doing your work is the task (occasionally drifting up to the project). But the task is too granular to be a useful unit of abstraction for a review. For that, you need to step up to the project level.

The purpose of this program is to allow you to store your stuff in taskwarrior (where the unit of abstraction is optimized for doing) and review your stuff in gtd-review (where the unit of abstraction is optimized for reviewing).

As such, this program works best when used in conjunction with the optional on-exit hook provided above. Using it with the on-exit hook ensures that no projects "slip through" because the only tasks tied to them in taskwarrior are completed or deleted before your review. The fundamental assumption that this program makes is that projects should never been deleted by a program, only by you. The on-exit hook ensure that this is true.

## Roadmap

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
