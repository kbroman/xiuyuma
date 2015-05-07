context("digitscount")
test_that("digitscount works for small vectors", {
	v<-c(0,0,0,1.2,265.7,345.6,78,89,-91,-90.9,0.1,-0.02)
	result<-table(factor(c(1,2,3,7,8,9,9,1,2),levels=1:9))
	expect_equal(digitscount(v),result)
})


