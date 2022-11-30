(require 'ack)

(add-to-list
 'ack-project-root-patterns
 "ext_emconf\\.php'")

(setq compilation-search-path '(nil))

(provide 'arvid-ack)
