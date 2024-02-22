# Dependencies ----
import jax.numpy as jnp
from jax import grad, jit, vmap, lax
from jax.numpy.linalg import norm
from jax import random as rand
from jax import config
from functools import partial


# Settings ----
config.update('jax_enable_x64', True)
key = rand.PRNGKey(0)


# Misc Functions ----
def generate_matrix(nobs, nfeat, key):
    # Initialize matrix
    mat = rand.normal(key= key, shape= (nobs, 1))

    # Append new variables
    for _ in jnp.arange(nfeat - 1):
        # Split the PRNG key
        keys = rand.split(key, 4)

        # Generate coefficients
        alpha = rand.normal(key= keys[0]) * 8
        beta = rand.normal(key= keys[1]) * 4
        s = rand.exponential(key= keys[2]) * 2

        # New variable = a + bX + err
        var = alpha + (beta * mat).sum(1) + s * rand.normal(key= keys[3], 
                                                            shape= (nobs, ))

        # Bind new column
        mat = jnp.column_stack([mat, var])

    return(mat)


# PCA Functions ----
# Project a set of (column) vectors onto another vector
@jit
def orth_proj(mat, vec):
    return( jnp.dot(mat, vec) )

# Gradient of projected variance w.r.t the input vector
gradient = grad(lambda mat, vec: orth_proj(mat, vec).var(), argnums = 1)

# Calculate first principal direction
@partial(jit, static_argnums = (1,2,3))
def find_pd(mat, nfeat, max_iter, tol, key):
    # Split the PRNG key
    key = rand.split(key, 1)[0]

    # Initialize random vector
    cur_v = rand.uniform(key, shape = (nfeat,))

    # Find principal direction
    def cond_fun(state):
        # Unpack last iteration
        cur_v, diff, iter = state

        # Check tolerance and iteration
        return( (diff > tol) & (iter < max_iter) )
    def body_fun(state):
        # Unpack last iteration
        cur_v, diff, iter = state
        iter += 1

        # Keep track of last iteration's vector
        old_v = cur_v.copy()

        # Update current vector with gradient
        cur_v = cur_v + gradient(mat, cur_v)
        cur_v = cur_v / norm(cur_v)
        
        # Difference to check if tolerance has been reached
        diff = norm(cur_v - old_v)

        return(cur_v, diff, iter)

    cur_v, _, iter = lax.while_loop(cond_fun, body_fun, (cur_v, 1, 0))

    # Return principal direction
    return( cur_v, iter )

# Remove rank of data-matrix by subtracting an outer product
@partial(jit, static_argnums = 1)
def remove_rank(mat, nobs, pd):
    # Remove rank-1 matrix from data-matrix
    mat = mat - jnp.outer(orth_proj(pd), pd)

    return(mat)

# Test Data ----
# Hyperparameters
nobs = 1000
nfeat = 2
tol = 1e-8
max_iter = 10

# Split PRNG key
key = rand.split(key, 1)[0]

# Generate matrix
mat = generate_matrix(nobs, nfeat, key)

pd, i = find_pd(mat, mat.shape[1], max_iter, tol, key)