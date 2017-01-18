;; Time-stamp: <2017-01-18 11:31:21 kmodi>

;; WolframAlpha
;; https://github.com/hsjunnesson/wolfram.el

(use-package wolfram
  :bind (:map modi-mode-map
         ("C-x / a" . wolfram-alpha))
  :config
  (progn
    ;; 1. First get a Wolfram ID (or developer account) at
    ;;    https://developer.wolframalpha.com/portal/signin.html
    ;;    Your "Wolfram ID" will be the email you used to sign up for the
    ;;    developer account.
    ;; 2. Once you sign in with the Wolfram ID at
    ;;    https://developer.wolframalpha.com/portal/myapps/, click on "Get an
    ;;    AppID" to get your Wolfram API or AppID.
    ;; 3. Follow the steps where you put in your app name and description, and
    ;;    you will end up with an AppID that looks like "ABCDEF-GHIJKLMNOP",
    ;;    where few of those characters could be numbers too.
    ;;
    ;; The "wolfram-api" file is supposed to contain this line:
    ;;     (setq wolfram-alpha-app-id "<YOUR_WOLFRAM_APP_ID>")
    (load (locate-user-emacs-file "wolfram-api") nil :nomessage)))


(provide 'setup-wolfram)
