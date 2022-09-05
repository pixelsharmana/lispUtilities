(defun tree-node (value)
  "Creates a single, free floating node of a tree"
  (list :parent nil :childs nil :value value))

(defun tree-is-rootp (node)
  (not (getf node :parent)))

(defun tree-node-degree (node))

(defun tree-node-level (node))

(defun tree-node-attach-child (parent child)
  (ntack (getf parent :childs) child)
  (setf (getf child :parent) parent))
