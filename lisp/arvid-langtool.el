
(setq langtool-language-tool-jar
      "/home/arvid/bin/LanguageTool-4.4/languagetool-commandline.jar")
(setq langtool-language-tool-server-jar
      "/home/arvid/bin/LanguageTool-4.4/languagetool-server.jar")
(setq langtool-server-user-arguments '("-p" "8082"))
(setq langtool-default-language "en-US")

(require 'langtool)

(provide 'arvid-langtool)
