(require 'solarized-definitions
         (locate-file "solarized-definitions.el" custom-theme-load-path
                      '("c" "")))

(create-solarized-theme solarized
                        solarized-description (solarized-color-definitions)
                        (solarized-variable-definitions))
