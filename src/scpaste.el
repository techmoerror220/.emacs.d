<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN">
<!-- saved from url=(0021)https://p.hagelb.org/ -->
<html ml-update="aware"><head><meta http-equiv="Content-Type" content="text/html; charset=windows-1252">
    <title>scpaste.el</title>
    <style type="text/css">
    <!--
      body {
        color: #000000;
        background-color: #ffffff;
      }
      .comment {
        /* font-lock-comment-face */
        color: #b22222;
      }
      .comment-delimiter {
        /* font-lock-comment-delimiter-face */
        color: #b22222;
      }
      .constant {
        /* font-lock-constant-face */
        color: #008b8b;
      }
      .doc {
        /* font-lock-doc-face */
        color: #8b2252;
      }
      .function-name {
        /* font-lock-function-name-face */
        color: #0000ff;
      }
      .keyword {
        /* font-lock-keyword-face */
        color: #a020f0;
      }
      .string {
        /* font-lock-string-face */
        color: #8b2252;
      }
      .type {
        /* font-lock-type-face */
        color: #228b22;
      }
      .variable-name {
        /* font-lock-variable-name-face */
        color: #a0522d;
      }
      .warning {
        /* font-lock-warning-face */
        color: #ff0000;
        font-weight: bold;
      }

      a {
        color: inherit;
        background-color: inherit;
        font: inherit;
        text-decoration: inherit;
      }
      a:hover {
        text-decoration: underline;
      }
    -->
    </style>
  </head>
  <body>
    <pre><span class="comment-delimiter">;;; </span><span class="comment">scpaste.el --- Paste to the web via scp.
</span>
<span class="comment-delimiter">;; </span><span class="comment">Copyright © 2008-2018 Phil Hagelberg and contributors
</span>
<span class="comment-delimiter">;; </span><span class="comment">Author: Phil Hagelberg
</span><span class="comment-delimiter">;; </span><span class="comment">URL: https://github.com/technomancy/scpaste
</span><span class="comment-delimiter">;; </span><span class="comment">Version: 0.6.5
</span><span class="comment-delimiter">;; </span><span class="comment">Created: 2008-04-02
</span><span class="comment-delimiter">;; </span><span class="comment">Keywords: convenience hypermedia
</span><span class="comment-delimiter">;; </span><span class="comment">EmacsWiki: SCPaste
</span><span class="comment-delimiter">;; </span><span class="comment">Package-Requires: ((htmlize "1.39"))
</span>
<span class="comment-delimiter">;; </span><span class="comment">This file is NOT part of GNU Emacs.
</span>
<span class="comment-delimiter">;;; </span><span class="comment">Commentary:
</span>
<span class="comment-delimiter">;; </span><span class="comment">This will place an HTML copy of a buffer on the web on a server
</span><span class="comment-delimiter">;; </span><span class="comment">that the user has shell access on.
</span>
<span class="comment-delimiter">;; </span><span class="comment">It's similar in purpose to services such as https://gist.github.com
</span><span class="comment-delimiter">;; </span><span class="comment">or https://pastebin.com, but it's much simpler since it assumes the user
</span><span class="comment-delimiter">;; </span><span class="comment">has an account on a publicly-accessible HTTP server. It uses `</span><span class="comment"><span class="constant">scp</span></span><span class="comment">'
</span><span class="comment-delimiter">;; </span><span class="comment">as its transport and uses Emacs' font-lock as its syntax
</span><span class="comment-delimiter">;; </span><span class="comment">highlighter instead of relying on a third-party syntax highlighter
</span><span class="comment-delimiter">;; </span><span class="comment">for which individual language support must be added one-by-one.
</span>
<span class="comment-delimiter">;;; </span><span class="comment">Install
</span>
<span class="comment-delimiter">;; </span><span class="comment">Requires htmlize; available at https://github.com/hniksic/emacs-htmlize
</span>
<span class="comment-delimiter">;; </span><span class="comment">Open the file and run `</span><span class="comment"><span class="constant">package-install-from-buffer</span></span><span class="comment">', or put it on your
</span><span class="comment-delimiter">;; </span><span class="comment">`</span><span class="comment"><span class="constant">load-path</span></span><span class="comment">' and add these to your config:
</span>
<span class="comment-delimiter">;; </span><span class="comment">(autoload 'scpaste "scpaste" nil t)
</span><span class="comment-delimiter">;; </span><span class="comment">(autoload 'scpaste-region "scpaste" nil t)
</span>
<span class="comment-delimiter">;; </span><span class="comment">Set `</span><span class="comment"><span class="constant">scpaste-http-destination</span></span><span class="comment">' and `</span><span class="comment"><span class="constant">scpaste-scp-destination</span></span><span class="comment">' to
</span><span class="comment-delimiter">;; </span><span class="comment">appropriate values, and add this to your Emacs config:
</span>
<span class="comment-delimiter">;; </span><span class="comment">(setq scpaste-http-destination "https://p.hagelb.org"
</span><span class="comment-delimiter">;;       </span><span class="comment">scpaste-scp-destination "p.hagelb.org:p.hagelb.org")
</span>
<span class="comment-delimiter">;; </span><span class="comment">If you have a different keyfile, you can set that, too:
</span><span class="comment-delimiter">;; </span><span class="comment">(setq scpaste-scp-pubkey "~/.ssh/my_keyfile.pub")
</span>
<span class="comment-delimiter">;; </span><span class="comment">If you use a non-standard ssh port, you can specify it by setting
</span><span class="comment-delimiter">;; </span><span class="comment">`</span><span class="comment"><span class="constant">scpaste-scp-port</span></span><span class="comment">'.
</span>
<span class="comment-delimiter">;; </span><span class="comment">If you need to use alternative scp and ssh programs, you can set
</span><span class="comment-delimiter">;; </span><span class="comment">`</span><span class="comment"><span class="constant">scpaste-scp</span></span><span class="comment">' and `</span><span class="comment"><span class="constant">scpaste-ssh</span></span><span class="comment">'. For example, scpaste works with the Putty
</span><span class="comment-delimiter">;; </span><span class="comment">suite on Windows if you set these to pscp and plink, respectively.
</span>
<span class="comment-delimiter">;; </span><span class="comment">Optionally you can set the displayed name for the footer and where
</span><span class="comment-delimiter">;; </span><span class="comment">it should link to:
</span>
<span class="comment-delimiter">;; </span><span class="comment">(setq scpaste-user-name "Technomancy"
</span><span class="comment-delimiter">;;       </span><span class="comment">scpaste-user-address "https://technomancy.us/")
</span>
<span class="comment-delimiter">;;; </span><span class="comment">Usage
</span>
<span class="comment-delimiter">;; </span><span class="comment">M-x scpaste, enter a name, and press return. The name will be
</span><span class="comment-delimiter">;; </span><span class="comment">incorporated into the URL by escaping it and adding it to the end
</span><span class="comment-delimiter">;; </span><span class="comment">of `</span><span class="comment"><span class="constant">scpaste-http-destination</span></span><span class="comment">'. The URL for the pasted file will be
</span><span class="comment-delimiter">;; </span><span class="comment">pushed onto the kill ring.
</span>
<span class="comment-delimiter">;; </span><span class="comment">You can autogenerate a splash page that gets uploaded as index.html
</span><span class="comment-delimiter">;; </span><span class="comment">in `</span><span class="comment"><span class="constant">scpaste-http-destination</span></span><span class="comment">' by invoking M-x scpaste-index. This
</span><span class="comment-delimiter">;; </span><span class="comment">will upload an explanation as well as a listing of existing
</span><span class="comment-delimiter">;; </span><span class="comment">pastes. If a paste's filename includes "private" it will be skipped.
</span>
<span class="comment-delimiter">;; </span><span class="comment">You probably want to set up SSH keys for your destination to avoid
</span><span class="comment-delimiter">;; </span><span class="comment">having to enter your password once for each paste. Also be sure the
</span><span class="comment-delimiter">;; </span><span class="comment">key of the host referenced in `</span><span class="comment"><span class="constant">scpaste-scp-destination</span></span><span class="comment">' is in your
</span><span class="comment-delimiter">;; </span><span class="comment">known hosts file--scpaste will not prompt you to add it but will
</span><span class="comment-delimiter">;; </span><span class="comment">simply hang and need you to hit C-g to cancel it.
</span>
<span class="comment-delimiter">;;; </span><span class="comment">License:
</span>
<span class="comment-delimiter">;; </span><span class="comment">This program is free software; you can redistribute it and/or modify
</span><span class="comment-delimiter">;; </span><span class="comment">it under the terms of the GNU General Public License as published by
</span><span class="comment-delimiter">;; </span><span class="comment">the Free Software Foundation; either version 3, or (at your option)
</span><span class="comment-delimiter">;; </span><span class="comment">any later version.
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">This program is distributed in the hope that it will be useful,
</span><span class="comment-delimiter">;; </span><span class="comment">but WITHOUT ANY WARRANTY; without even the implied warranty of
</span><span class="comment-delimiter">;; </span><span class="comment">MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
</span><span class="comment-delimiter">;; </span><span class="comment">GNU General Public License for more details.
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">You should have received a copy of the GNU General Public License
</span><span class="comment-delimiter">;; </span><span class="comment">along with GNU Emacs; see the file COPYING.  If not, write to the
</span><span class="comment-delimiter">;; </span><span class="comment">Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
</span><span class="comment-delimiter">;; </span><span class="comment">Boston, MA 02110-1301, USA.
</span>
<span class="comment-delimiter">;;; </span><span class="comment">Code:
</span>
(<span class="keyword">require</span> '<span class="constant">url</span>)
(<span class="keyword">require</span> '<span class="constant">htmlize</span>)

(<span class="keyword">defvar</span> <span class="variable-name">scpaste-scp-port</span>
  nil)

(<span class="keyword">defvar</span> <span class="variable-name">scpaste-scp</span>
  <span class="string">"scp"</span>
  <span class="doc">"The scp program to use."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">scpaste-ssh</span>
  <span class="string">"ssh"</span>
  <span class="doc">"The ssh program to use when running remote shell commands."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">scpaste-http-destination</span>
  <span class="string">"https://p.hagelb.org"</span>
  <span class="doc">"Publicly-accessible (via HTTP) location for pasted files."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">scpaste-scp-destination</span>
  <span class="string">"p.hagelb.org:p.hagelb.org"</span>
  <span class="doc">"SSH-accessible directory corresponding to `</span><span class="doc"><span class="constant">scpaste-http-destination</span></span><span class="doc">'.
You must have write access to this directory via `</span><span class="doc"><span class="constant">scp</span></span><span class="doc">'."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">scpaste-scp-pubkey</span>
  nil
  <span class="doc">"Identity file for the server.
Corresponds to ssh’s `-i` option Example: \"~/.ssh/id.pub\".
It's better to set this in ~/.ssh/config than to use this setting."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">scpaste-user-name</span>
  nil
  <span class="doc">"Name displayed under the paste."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">scpaste-user-address</span>
  nil
  <span class="doc">"Link to the user’s homebase (can be a mailto:)."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">scpaste-make-name-function</span>
  'scpaste-make-name-from-buffer-name
  <span class="doc">"The function used to generate file names, unless the user provides one."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">scpaste-el-location</span> (replace-regexp-in-string <span class="string">"\.elc$"</span> <span class="string">".el"</span>
                                                      (<span class="keyword">or</span> load-file-name
                                                          (buffer-file-name))))

(<span class="keyword">defun</span> <span class="function-name">scpaste-footer</span> ()
  <span class="doc">"HTML message to place at the bottom of each file."</span>
  (concat <span class="string">"&lt;p style='font-size: 8pt; font-family: monospace; "</span>
          (mapconcat (<span class="keyword">lambda</span> (c) (concat c <span class="string">"-select: none"</span>))
                     '(<span class="string">"-moz-user"</span> <span class="string">"-webkit-user"</span> <span class="string">"-ms-user"</span> <span class="string">"user"</span>) <span class="string">"; "</span>)
          <span class="string">"'&gt;Generated by "</span>
          (<span class="keyword">let</span> ((user (<span class="keyword">or</span> scpaste-user-name user-full-name)))
            (<span class="keyword">if</span> scpaste-user-address
                (concat <span class="string">"&lt;a href='"</span> scpaste-user-address <span class="string">"'&gt;"</span> user <span class="string">"&lt;/a&gt;"</span>)
              user))
          <span class="string">" using &lt;a href='https://p.hagelb.org'&gt;scpaste&lt;/a&gt; at %s. "</span>
          (cadr (current-time-zone)) <span class="string">". (&lt;a href='%s'&gt;original&lt;/a&gt;)&lt;/p&gt;"</span>))

(<span class="keyword">defun</span> <span class="function-name">scpaste-read-name</span> (<span class="type">&amp;optional</span> suffix)
  <span class="doc">"Read the paste name from the minibuffer.

Defaults to the return value of `</span><span class="doc"><span class="constant">scpaste-make-name-function</span></span><span class="doc">'
with SUFFIX as argument."</span>
  (<span class="keyword">let*</span> ((default (funcall scpaste-make-name-function suffix))
         (input (read-from-minibuffer (format <span class="string">"Name: (defaults to %s) "</span>
                                              default))))
    (<span class="keyword">if</span> (equal <span class="string">""</span> input) default input)))

(<span class="keyword">defun</span> <span class="function-name">scpaste-make-name-from-buffer-name</span> (<span class="type">&amp;optional</span> suffix)
  <span class="doc">"Make a name from buffer name and extension.

If non-nil, SUFFIX is inserted between name and extension."</span>
  (concat (file-name-sans-extension (buffer-name))
          suffix
          (file-name-extension (buffer-name) t)))

(<span class="keyword">defun</span> <span class="function-name">scpaste-make-name-from-timestamp</span> (<span class="type">&amp;optional</span> _)
  <span class="doc">"Make a name from current timestamp and current buffer's extension."</span>
  (concat (format-time-string <span class="string">"%s"</span>) (file-name-extension (buffer-name) t)))

<span class="comment-delimiter">;;;</span><span class="comment">###</span><span class="comment"><span class="warning">autoload</span></span><span class="comment">
</span>(<span class="keyword">defun</span> <span class="function-name">scpaste</span> (original-name)
  <span class="doc">"Paste the current buffer via `</span><span class="doc"><span class="constant">scp</span></span><span class="doc">' to `</span><span class="doc"><span class="constant">scpaste-http-destination</span></span><span class="doc">'.
If ORIGINAL-NAME is an empty string, then the buffer name is used
for the file name."</span>
  (<span class="keyword">interactive</span> (list (scpaste-read-name)))
  (<span class="keyword">let*</span> ((b (generate-new-buffer (generate-new-buffer-name <span class="string">"scpaste"</span>)))
         (pre-hl-line (<span class="keyword">and</span> (<span class="keyword">featurep</span> '<span class="constant">hl-line</span>) hl-line-mode
                           (<span class="keyword">progn</span> (hl-line-mode -1) t)))
         (hb (htmlize-buffer))
         (name (replace-regexp-in-string <span class="string">"[/\\%*:|\"&lt;&gt;  ]+"</span> <span class="string">"_"</span>
                                         original-name))
         (full-url (concat scpaste-http-destination
                           <span class="string">"/"</span> (url-hexify-string name) <span class="string">".html"</span>))
         (tmp-file (concat temporary-file-directory name))
         (tmp-hfile (concat temporary-file-directory name <span class="string">".html"</span>)))
    (<span class="keyword">when</span> pre-hl-line
      (hl-line-mode 1))
    <span class="comment-delimiter">;; </span><span class="comment">Save the files (while adding a footer to html file)
</span>    (<span class="keyword">save-excursion</span>
      (copy-to-buffer b (point-min) (point-max))
      (switch-to-buffer b)
      (write-file tmp-file)
      (kill-buffer b)
      (switch-to-buffer hb)
      (goto-char (point-min))
      (search-forward <span class="string">"&lt;/body&gt;\n&lt;/html&gt;"</span>)
      (insert (format (scpaste-footer)
                      (current-time-string)
                      (substring full-url 0 -5)))
      (write-file tmp-hfile)
      (kill-buffer hb))

    (<span class="keyword">let*</span> ((identity (<span class="keyword">if</span> scpaste-scp-pubkey
                         (concat <span class="string">"-i "</span> scpaste-scp-pubkey) <span class="string">""</span>))
           (port (<span class="keyword">if</span> scpaste-scp-port (concat <span class="string">"-P "</span> scpaste-scp-port)))
           (invocation (concat scpaste-scp <span class="string">" -q "</span> identity <span class="string">" "</span> port))
           (command (concat invocation <span class="string">" "</span> tmp-file <span class="string">" "</span> tmp-hfile <span class="string">" "</span>
                            scpaste-scp-destination <span class="string">"/"</span>))
           (error-buffer <span class="string">"*scp-error*"</span>)
           (retval (shell-command command nil error-buffer))
           (select-enable-primary t))

      (delete-file tmp-file)
      (delete-file tmp-hfile)
      <span class="comment-delimiter">;; </span><span class="comment">Notify user and put the URL on the kill ring
</span>      (<span class="keyword">if</span> (= retval 0)
          (<span class="keyword">progn</span> (kill-new full-url)
                 (message <span class="string">"Pasted to %s (on kill ring)"</span> full-url))
        (pop-to-buffer error-buffer)
        (help-mode-setup)))))

<span class="comment-delimiter">;;;</span><span class="comment">###</span><span class="comment"><span class="warning">autoload</span></span><span class="comment">
</span>(<span class="keyword">defun</span> <span class="function-name">scpaste-region</span> (name)
  <span class="doc">"Paste the current region via `</span><span class="doc"><span class="constant">scpaste</span></span><span class="doc">'.
NAME is used for the file name."</span>
  (<span class="keyword">interactive</span> (list (scpaste-read-name (format <span class="string">"-%s-%s"</span> (region-beginning)
                                                (region-end)))))
  (<span class="keyword">let</span> ((region-contents (buffer-substring (mark) (point))))
    (<span class="keyword">with-temp-buffer</span>
      (insert region-contents)
      (scpaste name))))

<span class="comment-delimiter">;;;</span><span class="comment">###</span><span class="comment"><span class="warning">autoload</span></span><span class="comment">
</span>(<span class="keyword">defun</span> <span class="function-name">scpaste-index</span> ()
  <span class="doc">"Generate an index of all existing pastes on server on the splash page."</span>
  (<span class="keyword">interactive</span>)
  (<span class="keyword">let*</span> ((dest-parts (split-string scpaste-scp-destination <span class="string">":"</span>))
         (files (shell-command-to-string (concat scpaste-ssh <span class="string">" "</span>
                                                 (car dest-parts) <span class="string">" ls "</span>
                                                 (cadr dest-parts))))
         (file-list (split-string files <span class="string">"\n"</span>)))
    (<span class="keyword">save-excursion</span>
      (<span class="keyword">with-temp-buffer</span>
        (insert-file-contents scpaste-el-location)
        (goto-char (point-min))
        (search-forward <span class="string">";;; Commentary"</span>)
        (forward-line -1)
        (insert <span class="string">"\n;;; Pasted Files\n\n"</span>)
        (<span class="keyword">dolist</span> (file file-list)
          (<span class="keyword">when</span> (<span class="keyword">and</span> (string-match <span class="string">"\\.html$"</span> file)
                     (not (string-match <span class="string">"private"</span> file)))
            (insert (concat <span class="string">";; * &lt;"</span> scpaste-http-destination <span class="string">"/"</span> file <span class="string">"&gt;\n"</span>))))
        (emacs-lisp-mode)
        (<span class="keyword">if</span> (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings <span class="comment-delimiter">; </span><span class="comment">fallback for Emacs 24
</span>            (font-lock-fontify-buffer)))
        (rename-buffer <span class="string">"SCPaste"</span>)
        (write-file (concat temporary-file-directory <span class="string">"scpaste-index"</span>))
        (scpaste <span class="string">"index"</span>)))))

(<span class="keyword">provide</span> '<span class="constant">scpaste</span>)
<span class="comment-delimiter">;;; </span><span class="comment">scpaste.el ends here
</span></pre>
  
<p style="font-size: 8pt; font-family: monospace; -moz-user-select: none; -webkit-user-select: none; -ms-user-select: none; user-select: none">Generated by Phil Hagelberg using <a href="https://p.hagelb.org/">scpaste</a> at Thu Apr  2 14:51:38 2020. PDT. (<a href="https://p.hagelb.org/index">original</a>)</p>
</body><div id="saka-gui-root" style="position: absolute; left: 0px; top: 0px; width: 100%; height: 100%; z-index: 2147483647; opacity: 1; pointer-events: none;"><div></div></div></html>