#include <Rcpp.h>
#include <RcppEigen.h>
#include <iostream>
#include <chrono>
#include <vector>
#include <boost/random.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/uniform_real_distribution.hpp>
#include "cartesian_geom/cartesian_kernel.h"
#include "random_walks/random_walks.hpp"
#include "sampling/sample_correlation_matrices.hpp"

//' Uniformly sample correlation matrices
//'
//' @param n The dimension of the correlation matrix.
//' @param num_matrices The number of matrices to generate.
//' @param walk_length The length of the random walk.
//' @param nburns The number of burn-in steps for the random walk.
//' @param validate Optional. Whether to validate the sampled matrices. Default is false.
//'
//' @return A list of sampled correlation matrices.

// [[Rcpp::export]]
Rcpp::List uniform_sample_correlation_matrices(const unsigned int n, const unsigned int num_matrices = 1000, 
                                               const unsigned int walk_length=1, const unsigned int nburns = 0,  const bool validate = false) {
    typedef double NT;
    typedef Eigen::Matrix<NT, Eigen::Dynamic, Eigen::Dynamic> MT;
    typedef BoostRandomNumberGenerator<boost::mt19937, NT> RNGType;
    typedef CorreMatrix<NT> PointMT;

    std::vector<PointMT> randPoints;

    uniform_correlation_sampling_MT<AcceleratedBilliardWalk, PointMT, RNGType>(n, randPoints, walk_length, num_matrices, nburns);

    const NT tol = 1e-8;

    std::vector<MT> sampled_correlation_matrices; 

    for (const auto& points : randPoints) {
        sampled_correlation_matrices.push_back(points.mat); // Store the sampled point

        if (validate) {
            // Check if all the diagonal elements are 1
            for (int i = 0; i < points.mat.rows(); i++) {
                if (std::abs(points.mat(i, i) - 1.0) > tol) {
                    throw Rcpp::exception("Invalid correlation matrix: not all diagonal elements are 1");
                }
            }

            // Check if the matrix is positive definite
            Eigen::SelfAdjointEigenSolver<MT> eigen_solver(points.mat);

            if (eigen_solver.info() != Eigen::Success) {
                throw Rcpp::exception("Invalid correlation matrix: matrix decomposition failed");
            }

            if (eigen_solver.eigenvalues().minCoeff() < tol) {
                throw Rcpp::exception("Invalid correlation matrix: matrix is not positive definite");
            }
        }
    }

    Rcpp::List rcpp_sampled_matrices(sampled_correlation_matrices.size());
    for (size_t i = 0; i < sampled_correlation_matrices.size(); ++i) {
        rcpp_sampled_matrices[i] = Rcpp::wrap(sampled_correlation_matrices[i]);
    }

    Rcpp::List result = Rcpp::List::create(
        Rcpp::Named("sampled_matrices") = rcpp_sampled_matrices
    );

    return result;
}
