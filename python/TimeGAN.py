import sys
import os
import glob

import tensorflow as tf
import pandas as pd
import numpy as np 

from sklearn.preprocessing import MinMaxScaler

from tensorflow.keras.models import Sequential, Model
from tensorflow.keras.layers import GRU, Dense, RNN, GRUCell, Input
from tensorflow.keras.losses import BinaryCrossentropy, MeanSquaredError
from tensorflow.keras.optimizers import Adam


arg1 = int(sys.argv[1])
arg2 = int(sys.argv[2])
arg3 = int(sys.argv[3])
arg4 = int(sys.argv[4])

# Initial parameters
n_obs = arg1 
seq_len = n_obs
batch_size = arg2
n_seq = arg3
hidden_dim = 12
n_layers = 3
train_steps = arg4
gamma = 1

# Initial settings

# files = glob.glob('data/*.csv')
files = glob.glob('python/data/*.csv') # Change it if not in RStudio

files.sort()

df = pd.DataFrame()

for i in range(len(files)):
    temp_df = pd.read_csv(files[i])
    df = pd.concat([df, temp_df], axis = 1)

# Secondary functions
def make_random_data():
    while True:
        yield np.random.uniform(low = 0, high = 1, size = (seq_len, n_seq))
        
random_series = (tf.data.Dataset.from_generator(make_random_data, output_types = tf.float32).batch(batch_size))
random_series_iter = iter(random_series.repeat())

def make_rnn(n_layers, hidden_units, output_units, name):
    return Sequential([GRU(units = hidden_units, return_sequences = True, name = f'GRU_{i + 1}') for i in range(n_layers)] +
                      [Dense(units = output_units, activation = 'sigmoid', name = 'OUT')], name = name)

def train_autoencoder_init(x):
    with tf.GradientTape() as tape:
        x_tilde = autoencoder(x)
        embedding_loss_t0 = mse(x, x_tilde)
        e_loss_0 = 10 * tf.sqrt(embedding_loss_t0)

    var_list = embedder.trainable_variables + recovery.trainable_variables
    gradients = tape.gradient(e_loss_0, var_list)
    autoencoder_optimizer.apply_gradients(zip(gradients, var_list))
    return tf.sqrt(embedding_loss_t0)

def train_supervisor(x):
    with tf.GradientTape() as tape:
        h = embedder(x)
        h_hat_supervised = supervisor(h)
        g_loss_s = mse(h[:, 1:, :], h_hat_supervised[:, 1: , :])

    var_list = supervisor.trainable_variables
    gradients = tape.gradient(g_loss_s, var_list)
    supervisor_optimizer.apply_gradients(zip(gradients, var_list))
    return g_loss_s

def get_generator_moment_loss(y_true, y_pred):
    y_true_mean, y_true_var = tf.nn.moments(x = y_true, axes = [0])
    y_pred_mean, y_pred_var = tf.nn.moments(x = y_pred, axes = [0])
    g_loss_mean = tf.reduce_mean(tf.abs(y_true_mean - y_pred_mean))
    g_loss_var = tf.reduce_mean(tf.abs(tf.sqrt(y_true_var + 1e-6) - tf.sqrt(y_pred_var + 1e-6)))
    return g_loss_mean + g_loss_var

def train_generator(x, z):
    with tf.GradientTape() as tape:
        y_fake = adversarial_supervised(z)
        generator_loss_unsupervised = bce(y_true = tf.ones_like(y_fake),
                                          y_pred = y_fake)

        y_fake_e = adversarial_emb(z)
        generator_loss_unsupervised_e = bce(y_true = tf.ones_like(y_fake_e),
                                            y_pred = y_fake_e)
        h = embedder(x)
        h_hat_supervised = supervisor(h)
        generator_loss_supervised = mse(h[:, 1:, :], h_hat_supervised[:, 1:, :])

        x_hat = synthetic_data(z)
        generator_moment_loss = get_generator_moment_loss(x, x_hat)

        generator_loss = (generator_loss_unsupervised +
                          generator_loss_unsupervised_e +
                          100 * tf.sqrt(generator_loss_supervised) +
                          100 * generator_moment_loss)

    var_list = generator.trainable_variables + supervisor.trainable_variables
    gradients = tape.gradient(generator_loss, var_list)
    generator_optimizer.apply_gradients(zip(gradients, var_list))
    return generator_loss_unsupervised, generator_loss_supervised, generator_moment_loss

def train_embedder(x):
    with tf.GradientTape() as tape:
        h = embedder(x)
        h_hat_supervised = supervisor(h)
        generator_loss_supervised = mse(h[:, 1:, :], h_hat_supervised[:, 1:, :])

        x_tilde = autoencoder(x)
        embedding_loss_t0 = mse(x, x_tilde)
        e_loss = 10 * tf.sqrt(embedding_loss_t0) + 0.1 * generator_loss_supervised

    var_list = embedder.trainable_variables + recovery.trainable_variables
    gradients = tape.gradient(e_loss, var_list)
    embedding_optimizer.apply_gradients(zip(gradients, var_list))
    return tf.sqrt(embedding_loss_t0)

def get_discriminator_loss(x, z):
    y_real = discriminator_model(x)
    discriminator_loss_real = bce(y_true = tf.ones_like(y_real),
                                  y_pred = y_real)

    y_fake = adversarial_supervised(z)
    discriminator_loss_fake = bce(y_true = tf.zeros_like(y_fake),
                                  y_pred = y_fake)

    y_fake_e = adversarial_emb(z)
    discriminator_loss_fake_e = bce(y_true = tf.zeros_like(y_fake_e),
                                    y_pred = y_fake_e)
    return (discriminator_loss_real + discriminator_loss_fake + gamma * discriminator_loss_fake_e)

def train_discriminator(x, z):
    with tf.GradientTape() as tape:
        discriminator_loss = get_discriminator_loss(x, z)

    var_list = discriminator.trainable_variables
    gradients = tape.gradient(discriminator_loss, var_list)
    discriminator_optimizer.apply_gradients(zip(gradients, var_list))
    return discriminator_loss

# Main code

# Scale data
scaler = MinMaxScaler()
scaled_data = scaler.fit_transform(df).astype(np.float32)

# Rolling window
data = []
for i in range(len(df) - seq_len):
    data.append(scaled_data[i:i + seq_len])
n_windows = len(data)

# Encapsulate the data into a proper tf.data.Dataset object
real_series = (tf.data.Dataset.from_tensor_slices(data).shuffle(buffer_size = n_windows).batch(batch_size)) 
real_series_iter = iter(real_series.repeat())

random_series = (tf.data.Dataset.from_generator(make_random_data, output_types = tf.float32).batch(batch_size))
random_series_iter = iter(random_series.repeat())

# Create the TimeGAN model components
X = Input(shape = [seq_len, n_seq], name = 'RealData')
Z = Input(shape = [seq_len, n_seq], name = 'RandomData')

embedder      = make_rnn(n_layers = n_layers, hidden_units = hidden_dim, output_units = hidden_dim, name = 'Embedder')
recovery      = make_rnn(n_layers = n_layers, hidden_units = hidden_dim, output_units = n_seq, name = 'Recovery')
generator     = make_rnn(n_layers = n_layers, hidden_units = hidden_dim, output_units = hidden_dim, name ='Generator')
discriminator = make_rnn(n_layers = n_layers, hidden_units = hidden_dim, output_units = 1, name = 'Discriminator')
supervisor    = make_rnn(n_layers = 2, hidden_units = hidden_dim, output_units = hidden_dim, name = 'Supervisor')

mse = MeanSquaredError()
bce = BinaryCrossentropy()

# Autoencoder training
H = embedder(X)
X_tilde = recovery(H)

autoencoder = Model(inputs = X, outputs = X_tilde, name = 'Autoencoder')
autoencoder_optimizer = Adam()

for step in range(train_steps):
    X_ = next(real_series_iter)
    step_e_loss_t0 = train_autoencoder_init(X_)
    
# Supervised training
supervisor_optimizer = Adam()

for step in range(train_steps):
    X_ = next(real_series_iter)
    step_g_loss_s = train_supervisor(X_)
    
# Joint model
E_hat = generator(Z)
H_hat = supervisor(E_hat)
Y_fake = discriminator(H_hat)

adversarial_supervised = Model(inputs = Z, outputs = Y_fake, name = 'AdversarialNetSupervised')
Y_fake_e = discriminator(E_hat)

adversarial_emb = Model(inputs = Z, outputs = Y_fake_e, name = 'AdversarialNet')
X_hat = recovery(H_hat)

synthetic_data = Model(inputs = Z, outputs = X_hat, name = 'SyntheticData')
Y_real = discriminator(H)

discriminator_model = Model(inputs = X, outputs = Y_real, name = 'DiscriminatorReal')

generator_optimizer = Adam()
discriminator_optimizer = Adam()
embedding_optimizer = Adam()

# Training loop
step_g_loss_u = step_g_loss_s = step_g_loss_v = step_e_loss_t0 = step_d_loss = 0
for step in range(train_steps):
    for kk in range(2):
        X_ = next(real_series_iter)
        Z_ = next(random_series_iter)

        step_g_loss_u, step_g_loss_s, step_g_loss_v = train_generator(X_, Z_)
        step_e_loss_t0 = train_embedder(X_)

    X_ = next(real_series_iter)
    Z_ = next(random_series_iter)
    step_d_loss = get_discriminator_loss(X_, Z_)
    if step_d_loss > 0.15:
        step_d_loss = train_discriminator(X_, Z_)

# Generate data
generated_data = []

Z_ = next(random_series_iter)
d = synthetic_data(Z_)
generated_data.append(d)

generated_data = np.array(np.vstack(generated_data))
generated_data = (scaler.inverse_transform(generated_data.reshape(-1, n_seq)).reshape(-1, seq_len, n_seq))

gd_shape = generated_data.shape

# path_sim = f'data/synthetic_data'
path_sim = f'python/data/synthetic_data' # Change it if not in RStudio

for i in range(gd_shape[2]):
    newfile = f'{path_sim}/region_{(i + 1)}.csv'
    with open(newfile, 'wb') as nf:
        np.savetxt(nf, generated_data[:, :, i].transpose(), delimiter = ',')
    
