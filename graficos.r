library("lattice")
library("viridisLite")
library("latex2exp")
 
inf_limit_providers <- 100
sup_limit_providers <- 2000
providers_step <- 10

inf_limit_clients <- 1000
sup_limit_clients <- 20000
clients_step <- 100

# contract breakdown percentage
beta_vals <- c(0.5)

# amount of microservices
m <- 10

providers <- seq(inf_limit_providers, sup_limit_providers, providers_step)
clients <- seq(inf_limit_clients, sup_limit_clients, clients_step)
data <- expand.grid(X=clients, Y=providers)
providers <- rep(providers, each = length(clients))

for(beta in beta_vals){
	# best case
	data$Z <- providers + clients * (beta * (providers + clients)/providers + 4)
	pdf(paste("melhor_caso-b", beta * 100, ".pdf", sep = ""))
	print(levelplot(Z ~ X*Y, data=data , cuts = 100, xlab="Active clients", ylab="Providers", main= TeX(sprintf('Number of issued transactions - Best Case - $\\beta$ = %.1f$', beta)), col.regions = rainbow(100), panel = panel.levelplot.raster))
	dev.off()

	# worst case (m > max p)
	data$Z <- providers + clients * ((providers^2 + 3 * providers)/2 + beta * (1 + clients) + 2)
	pdf(paste("pior_caso-b", beta * 100, ".pdf", sep = ""))
	print(levelplot(Z ~ X*Y, data=data , cuts = 100, xlab="Active clients", ylab="Providers", main=TeX(sprintf("Number of issued transactions - Worst Case (m $\\geq$ p) - $\\beta = %.1f", beta)), col.regions = rainbow(100), panel = panel.levelplot.raster))
	dev.off()

	# worst case (m < p)
	data$Z <- providers + clients * ((m * (2 * providers - m + 3))/2 + beta * (1 + (m * clients)/providers) + 2)
	pdf(paste("pior_caso_menor-b", beta * 100, ".pdf", sep = ""))
	print(levelplot(Z ~ X*Y, data=data , cuts = 100, xlab="Active clients", ylab="Providers", main=TeX(sprintf("Number of issued transactions - Worst Case (m < p) - $\\beta = %.1f", beta)), col.regions = rainbow(100), panel = panel.levelplot.raster))
	dev.off()

	# worst case (mix)
	m <- 500
	lower_m <- providers + clients * ((providers^2 + 3 * providers)/2 + beta * (1 + clients) + 2)
	upper_m <- providers + clients * ((m * (2 * providers - m + 3))/2 + beta * (1 + (m * clients)/providers) + 2)
	threshold <- sum(providers < m)
	data$Z <- c(lower_m[1:threshold], upper_m[(threshold + 1):length(data$X)])

	pdf(paste("pior_caso_mix-b", beta * 100, ".pdf", sep = ""))
	print(levelplot(Z ~ X*Y, data=data , cuts = 100, xlab="Active clients", ylab="Providers", main=TeX(sprintf("Number of issued transactions - Worst Case (m = 500) - $\\beta = %.1f", beta)), col.regions = rainbow(100), panel = panel.levelplot.raster))
	dev.off()


}
