
# Install SciPy dependencies
sudo apt-get install libblas-dev liblapack-dev libatlas-base-dev gfortran

# Install pyenv
curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash

# Install python 3.5.0
env PYTHON_CONFIGURE_OPTS="--enable-shared" pyenv install 3.5.0

# Install cuda
wget http://developer.download.nvidia.com/compute/cuda/7.5/Prod/local_installers/cuda-repo-ubuntu1404-7-5-local_7.5-18_amd64.deb
sudo dpkg -i cuda-repo-ubuntu1404-7-5-local_7.5-18_amd64.deb
sudo apt-get update
sudo apt-get install cuda
popd
