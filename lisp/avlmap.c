///-------------------------------------------------------------------
/// Copyright (c) 2009-2011 by Evgeny Khirin.
///-------------------------------------------------------------------
///-------------------------------------------------------------------
/// File    : avlmap.cpp
/// Author  : Evgeny Khirin <>
/// Description : KLisp avlmap, based on AVL binary search trees.
///-------------------------------------------------------------------
#include "klisp.h"

//===================================================================
// NODES
//===================================================================
//-------------------------------------------------------------------
// node_t
//-------------------------------------------------------------------
typedef struct node_t node_t;

struct node_t {
  term      key;
  term      value;
  node_t *  left;
  node_t *  right;
  int       height;
};

//-------------------------------------------------------------------
// Internal function
//-------------------------------------------------------------------
static node_t * make_node(term key, term value) {
  node_t * n = (node_t *)lisp_alloc(sizeof(node_t), NULL);
  n->key = key;
  n->value = value;
  n->left = n->right = NULL;
  n->height = 1;
  return n;
}

//-------------------------------------------------------------------
// get height of the tree
//-------------------------------------------------------------------
static int height(struct node_t * n) {
  if (n == NULL) {
    return 0;
  }
  return n->height;
}

//-------------------------------------------------------------------
// get maximum of two integers
//-------------------------------------------------------------------
static int max(int a, int b) {
  return (a > b) ? a : b;
}

//-------------------------------------------------------------------
// Get Balance factor of node N
//-------------------------------------------------------------------
static int get_balance(node_t * n) {
  if (n == NULL) {
    return 0;
  }
  return height(n->left) - height(n->right);
}

/*-------------------------------------------------------------------
// nodes rotation
// T1, T2 and T3 are subtrees of the tree rooted with y (on left side)
// or x (on right side)
//                 y                               x
//                / \     Right Rotation          /    \
//               x   T3   – - – - – - – >        T1   y
//              / \       < - - - - - - -            /   \
//             T1  T2     Left Rotation            T2  T3
// Keys in both of the above trees follow the following order
//       keys(T1) < key(x) < keys(T2) < key(y) < keys(T3)
//-------------------------------------------------------------------*/

//-------------------------------------------------------------------
// right rotate subtree rooted with y
// See the diagram given above.
//-------------------------------------------------------------------
static node_t * right_rotate(node_t * y) {
  node_t * x = y->left;
  node_t * T2 = x->right;
  // Perform rotation
  x->right = y;
  y->left = T2;
  // Update heights
  y->height = max(height(y->left), height(y->right)) + 1;
  x->height = max(height(x->left), height(x->right)) + 1;
  // Return new root
  return x;
}

//-------------------------------------------------------------------
// left rotate subtree rooted with x
// See the diagram given above.
//-------------------------------------------------------------------
static node_t * left_rotate(node_t * x) {
  node_t * y = x->right;
  node_t * T2 = y->left;
  // Perform rotation
  y->left = x;
  x->right = T2;
  //  Update heights
  x->height = max(height(x->left), height(x->right)) + 1;
  y->height = max(height(y->left), height(y->right)) + 1;
  // Return new root
  return y;
}

//-------------------------------------------------------------------
// Insert params
//-------------------------------------------------------------------
typedef struct insert_params insert_params;

struct insert_params {
  term  key;
  term  value;
  term  compare_fn;
  term  old_value;
  int   is_new_key;
};

/*-------------------------------------------------------------------
// Steps to follow for insertion
// Let the newly nserted node be w
// 1) Perform standard BST insert for w.
// 2) Starting from w, travel up and find the first unbalanced node. Let z be
//    the first unbalanced node, y be the child of z that comes on the path from
//    w to z and x be the grandchild of z that comes on the path from w to z.
// 3) Re-balance the tree by performing appropriate rotations on the subtree
//    rooted with z. There can be 4 possible cases that needs to be handled as
//    x, y and z can be arranged in 4 ways. Following are the possible 4
//    arrangements:
//      a) y is left child of z and x is left child of y (Left Left Case)
//      b) y is left child of z and x is right child of y (Left Right Case)
//      c) y is right child of z and x is right child of y (Right Right Case)
//      d) y is right child of z and x is left child of y (Right Left Case)
//
// Following are the operations to be performed in above mentioned 4 cases. In
// all of the cases, we only need to re-balance the subtree rooted with z and
// the complete tree becomes balanced as the height of subtree (After
// appropriate rotations) rooted with z becomes same as it was before
// insertion. (See this video lecture for proof)
//
// a) Left Left Case
//
// T1, T2, T3 and T4 are subtrees.
//          z                                      y
//         / \                                   /   \
//        y   T4      Right Rotate (z)          x      z
//       / \          - - - - - - - - ->      /  \    /  \
//      x   T3                               T1  T2  T3  T4
//     / \
//   T1   T2
//
// b) Left Right Case
//
//      z                               z                           x
//     / \                            /   \                        /  \
//    y   T4  Left Rotate (y)        x    T4  Right Rotate(z)    y      z
//   / \      - - - - - - - - ->    /  \      - - - - - - - ->  / \    / \
// T1   x                          y    T3                    T1  T2 T3  T4
//     / \                        / \
//   T2   T3                    T1   T2
//
// c) Right Right Case
//
//   z                                y
//  /  \                            /   \
// T1   y     Left Rotate(z)       z      x
//     /  \   - - - - - - - ->    / \    / \
//    T2   x                     T1  T2 T3  T4
//        / \
//      T3  T4
//
// d) Right Left Case
//
//    z                            z                            x
//   / \                          / \                          /  \
// T1   y   Right Rotate (y)    T1   x      Left Rotate(z)   z      y
//     / \  - - - - - - - - ->     /  \   - - - - - - - ->  / \    / \
//    x   T4                      T2   y                  T1  T2  T3  T4
//   / \                              /  \
// T2   T3                           T3   T4
//
// C implementation.
// Following is the C implementation for AVL Tree Insertion. The following C
// implementation uses the recursive BST insert to insert a new node. In the
// recursive BST insert, after insertion, we get pointers to all ancestors one
// by one in bottom up manner. So we don’t need parent pointer to travel up. The
// recursive code itself travels up and visits all the ancestors of the newly
// inserted node.
// 1) Perform the normal BST insertion.
// 2) The current node must be one of the ancestors of the newly inserted
//    node. Update the height of the current node.
// 3) Get the balance factor (left subtree height – right subtree height) of the
//    current node.
// 4) If balance factor is greater than 1, then the current node is unbalanced
//    and we are either in Left Left case or left Right case. To check whether
//    it is left left case or not, compare the newly inserted key with the key
//    in left subtree root.
// 5) If balance factor is less than -1, then the current node is unbalanced and
//    we are either in Right Right case or Right Left case. To check whether it
//    is Right Right case or not, compare the newly inserted key with the key in
//    right subtree root.
//-------------------------------------------------------------------*/
static node_t * insert(node_t * node, insert_params * p) {
  // 1.  Perform the normal BST insertion
  if (node == NULL) {
    p->is_new_key = 1;
    return make_node(p->key, p->value);
  }
  long cmp_res = __fixnum_term_to_long(FUNCALL(p->compare_fn, p->key, node->key));
  if (cmp_res == 0) {
    p->old_value = node->value;
    node->value = p->value;
    return node;
  }
  if (cmp_res < 0) {
    node->left  = insert(node->left, p);
  } else {
    node->right = insert(node->right, p);
  }
  // 2. Update height of this ancestor node
  node->height = max(height(node->left), height(node->right)) + 1;
  // 3. Get the balance factor of this ancestor node to check whether this node
  //    became unbalanced.
  int balance = get_balance(node);
  // If this node becomes unbalanced, then there are 4 cases
  if (balance > 1) {
    cmp_res = __fixnum_term_to_long(FUNCALL(p->compare_fn, p->key, node->left->key));
    if (cmp_res < 0) {
      // Left Left Case
      return right_rotate(node);
    }
    // Left Right Case
    node->left =  left_rotate(node->left);
    return right_rotate(node);
  }
  if (balance < -1) {
    cmp_res = __fixnum_term_to_long(FUNCALL(p->compare_fn, p->key, node->right->key));
    if (cmp_res > 0) {
      // Right Right Case
      return left_rotate(node);
    }
    // Right Left Case
    node->right = right_rotate(node->right);
    return left_rotate(node);
  }
  // return unchanged node pointer
  return node;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static node_t * find(node_t * node, term key, term compare_fn) {
  long cmp_res;
 again:
  if (node == NULL) {
    return NULL;
  }
  cmp_res = __fixnum_term_to_long(FUNCALL(compare_fn, key, node->key));
  if (cmp_res == 0) {
    return node;
  }
  if (cmp_res < 0) {
    node = node->left;
    goto again;
  }
  node = node->right;
  goto again;
}

//-------------------------------------------------------------------
// Remove params
//-------------------------------------------------------------------
typedef struct remove_params remove_params;

struct remove_params {
  term  key;
  term  compare_fn;
  term  removed_value;
  int   is_key_removed;
};

//-------------------------------------------------------------------
// Given a non-empty binary search tree, return the node with minimum
// key value found in that tree. Note that the entire tree does not
// need to be searched.
//-------------------------------------------------------------------
static node_t * min_node(node_t * n) {
  // loop down to find the leftmost leaf
  while (n->left != NULL) {
    n = n->left;
  }
  return n;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
static node_t * remove_node(node_t * root, remove_params * p) {
  // STEP 1: Perform standard BST delete
  if (root == NULL) {
    return NULL;
  }
  long cmp_res = __fixnum_term_to_long(FUNCALL(p->compare_fn, p->key, root->key));
  // If the key to be deleted is smaller than the root's key,
  // then it lies in left subtree
  if (cmp_res < 0) {
    root->left = remove_node(root->left, p);
  } else if (cmp_res > 0) {
    // If the key to be deleted is greater than the root's key,
    // then it lies in right subtree
    root->right = remove_node(root->right, p);
  } else {
    // if key is same as root's key, then This is the node to be deleted
    // node with only one child or no child
    p->is_key_removed = 1;
    p->removed_value = root->value;
    if ((root->left == NULL) || (root->right == NULL)) {
      node_t * temp = root->left ? root->left : root->right;
      // No child case
      if(temp == NULL) {
        return NULL;
      }
      // One child case
      *root = *temp; // Copy the contents of the non-empty child
    } else {
      // node with two children: Get the inorder successor (smallest
      // in the right subtree)
      node_t * mnode = min_node(root->right);
      root->key = mnode->key;
      root->value = mnode->value;
      // Delete the inorder successor
      remove_params p1;
      p1 = *p;
      p1.key = root->key;
      root->right = remove_node(root->right, &p1);
    }
  }
  // STEP 2: UPDATE HEIGHT OF THE CURRENT NODE
  root->height = max(height(root->left), height(root->right)) + 1;
  // STEP 3: GET THE BALANCE FACTOR OF THIS NODE (to check whether
  //  this node became unbalanced)
  int balance = get_balance(root);
  // If this node becomes unbalanced, then there are 4 cases
  if (balance > 1 && get_balance(root->left) >= 0) {
    // Left Left Case
    return right_rotate(root);
  }
  if (balance > 1 && get_balance(root->left) < 0) {
    // Left Right Case
    root->left =  left_rotate(root->left);
    return right_rotate(root);
  }
  if (balance < -1 && get_balance(root->right) <= 0) {
    // Right Right Case
    return left_rotate(root);
  }
  // Right Left Case
  if (balance < -1 && get_balance(root->right) > 0) {
    root->right = right_rotate(root->right);
    return left_rotate(root);
  }
  return root;
}

//-------------------------------------------------------------------
// internal function
//-------------------------------------------------------------------
typedef void * (* walk_callback_t)(node_t *, void *);

static void * walk_tree(node_t * root, walk_callback_t fn, void * args) {
  if (root == NULL) {
    return args;
  }
  args = walk_tree(root->left, fn, args);
  args = fn(root, args);
  return walk_tree(root->right, fn, args);
}

//===================================================================
// TREE
//===================================================================
//-------------------------------------------------------------------
// tree_t
//-------------------------------------------------------------------
typedef struct tree_t tree_t;

struct tree_t {
  term     compare_fn;
  node_t * root;
  long     size;
  int      immutable;
};

//------------------------------------------------------------------------
// tree_init
//------------------------------------------------------------------------
static void tree_init(tree_t * t, term compare_fn) {
  t->compare_fn = compare_fn;
  t->root = NULL;
  t->size = 0;
  t->immutable = 0;
}

//------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------
static const tree_t * get_tree_for_read(term tbl) {
  return (const tree_t *)term_custom_value(tbl, g_avlmap_class_name);
}

//------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------
static tree_t * get_tree_for_write(term tbl) {
  tree_t * m = (tree_t *)term_custom_value(tbl, g_avlmap_class_name);
  if (m->immutable) {
    lisp_signal(g_immutable_object, tbl);
  }
  return m;
}

//------------------------------------------------------------------------
// Function: (avlmap-create &optional (test 'compare)) ==> avlmap
// Test is (lambda (x y)) and must return negative fixnum if x < y, zero if x = y
// and positive fixnum if x > y.
//------------------------------------------------------------------------
term avlmap_create(term test) {
  tree_t * m = (tree_t *)lisp_alloc(sizeof(tree_t), NULL);
  tree_init(m, test);
  term t = make_custom(g_avlmap_class_name, m);
  return t;
}

//------------------------------------------------------------------------
// Function: (avlmap-insert tbl key value &optional no-old-value) => old-value
//------------------------------------------------------------------------
term avlmap_insert(term tbl, term key, term value, term no_old_value) {
  tree_t * t = get_tree_for_write(tbl);
  insert_params p;
  p.key = key;
  p.value = value;
  p.old_value = no_old_value;
  p.is_new_key = 0;
  p.compare_fn = t->compare_fn;
  t->root = insert(t->root, &p);
  if (p.is_new_key) {
    t->size += 1;
    __deep_immune_literals(key);
  }
  return p.old_value;
}

//------------------------------------------------------------------------
// Function: (avlmap-lookup tbl key (not-found nil)) =>  object
//------------------------------------------------------------------------
term avlmap_lookup(term tbl, term key, term not_found) {
  const tree_t * t = get_tree_for_read(tbl);
  const node_t * n = find(t->root, key, t->compare_fn);
  if (n == NULL) {
    return not_found;
  }
  return n->value;
}

//------------------------------------------------------------------------
// Function: (avlmap-remove tbl key &optional no-old-value) => old-value
//------------------------------------------------------------------------
term avlmap_remove(term tbl, term key, term no_old_value) {
  tree_t * t = get_tree_for_write(tbl);
  remove_params p;
  p.key = key;
  p.compare_fn = t->compare_fn;
  p.removed_value = no_old_value;
  p.is_key_removed = 0;
  t->root = remove_node(t->root, &p);
  if (p.is_key_removed) {
    t->size -= 1;
    __deep_immune_literals(key);
  }
  return p.removed_value;
}

//------------------------------------------------------------------------
// Function: (avlmap-size tbl) => integer
//------------------------------------------------------------------------
term avlmap_size(term tbl) {
  return long_to_term(get_tree_for_read(tbl)->size);
}

//------------------------------------------------------------------------
// Function: (avlmap-clear tbl) => tbl
//------------------------------------------------------------------------
term avlmap_clear(term tbl) {
  tree_t * m = get_tree_for_write(tbl);
  m->root = NULL;
  m->size = 0;
  return tbl;
}

//------------------------------------------------------------------------
// internal function
//------------------------------------------------------------------------
static void * to_vector_callback(node_t * n, void * args) {
  vector_t * v = (vector_t *)args;
  v->data[v->size] = cons(n->key, n->value);
  v->size += 1;
  return v;
}

//------------------------------------------------------------------------
// Function: (avlmap-to-vector tbl &optional vec) ==> vector
// Each element of vector is cons, where car is key and cdr is value.
//------------------------------------------------------------------------
term avlmap_to_vector(term tbl, term vec) {
  const tree_t * m = get_tree_for_read(tbl);
  term res = is_null(vec) ? make_vector() : vec;
  vector_t * v = __get_vector_for_write(res);
  vector_ensure_capacity(v, m->size);
  walk_tree(m->root, to_vector_callback, v);
  return res;
}

