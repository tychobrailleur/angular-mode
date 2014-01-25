Minor mode for AngularJS.  This mode depends on helm.

# Installation

- Clone repository in your favourite location, e.g `.emacs.d`
- Add the following to your Emacs init file:

```
(add-to-list 'load-path "~/.emacs.d/angular-mode")
(require 'angular-mode)
```

# Bindings

- `C-c t`: Toggle test.  From a JavaScript file, switches to the unit
test, and vice-versa.
- `C-c c`: List controllers using Helm.
- `C-c d`: List directives using Helm.
- `C-c s`: List services using Helm.
- `C-c v`: List views using Helm.
- `C-c a`: List all.

# License

Copyright (c) 2014 Sébastien Le Callonnec

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
