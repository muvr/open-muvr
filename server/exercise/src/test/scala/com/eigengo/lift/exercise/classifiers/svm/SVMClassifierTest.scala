package com.eigengo.lift.exercise.classifiers.svm

import breeze.linalg.{DenseVector, DenseMatrix}
import com.eigengo.lift.exercise.classifiers.svm.SVMClassifier.SVMScale
import org.scalatest.{FlatSpec, Matchers}

class SVMClassifierTest extends FlatSpec with Matchers with SVMClassifier {

  "SVM preprocessing pipeline" should "transform the incomming data" in {
    val input = DenseMatrix((1d, 2d, 3d), (4d, 5d, 6d), (7d, 8d, 9d))
    val center = DenseVector[Double](1, 2, 3, 4, 5, 6, 7, 8, 9)
    val scale = DenseVector[Double](1, 2, 3, 4, 5, 6, 7, 8, 9)

    val result = preprocessingPipeline(input, Some(SVMScale(center, scale)))

    result should be(DenseVector(0.0, 1.0, 1.3333333333333333, -0.5, 0.0, 0.3333333333333333, -0.5714285714285714, -0.25, 0.0))
  }
}
