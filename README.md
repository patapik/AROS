# AROS (ApolloOS Flavour) Git Repository

## Contributing

Please see the [CONTRIBUTING.md](CONTRIBUTING.md) file for details on joining the GitHub organization, and guidelines on contributing to the AROS project.

## License

This project is licensed under the APL License - see the [LICENSE](LICENSE) file for details

## Acknowledgments

AROS contains parts built upon external components - see the [ACKNOWLEDGEMENTS](ACKNOWLEDGEMENTS) file for details

How to install ApolloOS on Linux 

Ubuntu 20.04.2.0 LTS example
(2.7GB iso) 2021.06.04
0. sudo apt-get update
steps:
1. sudo apt install build-essential
2. sudo apt-get install autotools-dev
3. sudo apt-get install bison
4. sudo apt-get install flex
5. sudo apt-get install netpbm
6. sudo apt-get install -y libpng-dev
7. sudo apt-get install ccache
8. sudo apt-get install git
9. sudo apt-get install automake
10. git clone https://github.com/ApolloTeam-dev/AROS.git
or 
git clone --recursive https://github.com/ApolloTeam-dev/AROS.git -b master ApolloOS_Master
11. cd Apollo*
12. sudo ./configure
13. sudo ./rebuild_all.sh
or
13. sudo ./mkapollo.sh all
or
	for the ROM: 
13. sudo ./mkapollo.sh --jobs=8 kernel
