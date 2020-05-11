(defun forge--repo-issues (repo)
  "Pull issues for given REPO and return them in org-element ast format."
  (let* ((id (oref repo id))
         (rows (forge-sql [:select $i1 :from issue :where (= repository $s2)]
                          [number title state author updated body]
                          id))
         (issues-url (replace-regexp-in-string
                      "https:" ""
                      (forge--format repo 'issues-url-format)))
         (org-data (seq-reduce
                    (lambda (acc row)
                      (pcase-let ((`(,number ,title ,state ,author ,updated ,body) row))
                        (let* ((issue (number-to-string number))
                               (issue-url (concat issues-url "/" issue)))
                          (append
                           acc
                           `((headline
                              (:title
                               ((link
                                 (:type "https" :path ,issue-url)
                                 ,(concat "#" issue))
                                ,(concat " " title))
                               :level 1)
                              (section
                               ()
                               (property-drawer
                                ()
                                (node-property (:key "Author" :value ,author))
                                (node-property (:key "Updated" :value ,updated))
                                (node-property (:key "State" :value ,state)))
                               (paragraph
                                ()
                                ,body
                                ))
                              ))))))
                    rows '())))
    (org-element-interpret-data
     `(org-data nil ,org-data))))

(defun forge-repo-issues->-org ()
  "Fetch git(hub) issues and display them in an Org-mode buffer."
  (interactive)
  (if-let ((repo (forge-get-repository nil)))
      (let* ((buf-n (forge--format repo 'name))
             (temp-buf (get-buffer-create buf-n)))
        (switch-to-buffer-other-window temp-buf)
        (with-current-buffer temp-buf
          (read-only-mode -1)
          (setf (buffer-string) "")
          (setf org-hide-emphasis-markers t)
          (funcall 'org-mode)
          (insert (forge--repo-issues repo))
          (org-indent-region (point-min) (point-max))
          (hide-sublevels 1)))
    (message "Cannot get issues, yet\nUse `M-x forge-add-repository' before trying again.")))
