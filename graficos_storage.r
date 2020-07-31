library("lattice")
library("viridisLite")
library("latex2exp")
 
inf_limit_providers <- 100
sup_limit_providers <- 2000
providers_step <- 10

inf_limit_clients <- 1000
sup_limit_clients <- 20000
clients_step <- 100

# transaction sizes in bytes

common_fields <- 110
cp <- common_fields
sr <- common_fields + 4
sp <- common_fields + 32 + 32 + 32 + 1 + 4 + 2
re <- common_fields + 32 + 2 
cc <- common_fields + 32 + 2
vc <- common_fields + 32 + 32 + 2 + 2 + 1 + 2 + 1 + 2 + 1 + 72 + 3
v <- common_fields + 32 + 32 + 2 + 1 + 72 + 3
vt <- common_fields + 32 + 33 + 2
cf <- common_fields + 32

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
	data$Z <- (providers * cp) + 
		  (3 * clients * (sr + sp + cc)) + 
		  (beta * clients * vc) + 
		  (beta * clients * (clients/providers - 1) * v) + 
		  (beta * clients * vt) + 
		  (clients * cf)
	# color scale represents megabytes
	data$Z <- data$Z/1024^2

	pdf(paste("data_melhor_caso-b", beta * 100, ".pdf", sep = ""))
	print(levelplot(Z ~ X*Y, data=data , cuts = 100, xlab="Active clients", ylab="Providers", main= TeX(sprintf("Storaged data size(MB) - Best Case - $\\beta = %.1f$", beta)), col.regions = rainbow(100), panel = panel.levelplot.raster))
	dev.off()

	# worst case (m > max p)
	data$Z <- (providers * cp) + 
		  (clients * sr) +
		  clients * (((providers^2 + providers)/2 * sp) + providers * (cc)) +
		  beta * clients * vc +
		  beta * clients * (clients - 1) * v +
		  beta * clients * vt +
		  clients * cf
	# color scale represents terabytes
	data$Z <- data$Z/1024^4

	pdf(paste("data_pior_caso-b", beta * 100, ".pdf", sep = ""))
	print(levelplot(Z ~ X*Y, data=data , cuts = 100, xlab="Active clients", ylab="Providers", main=TeX(sprintf("Storaged data size(TB) - Worst Case (m $\\geq$ p) - $\\beta = %.1f$", beta)), col.regions = rainbow(100), panel = panel.levelplot.raster))
	dev.off()

	# worst case (m < p)
	data$Z <- (providers * cp) + 
		  clients * sr +
		  clients * (((2 * providers * m - m^2 -m)/2) * sp + m * cc) +
		  beta * clients * vc +
		  beta * clients * ((m * clients)/providers - 1) * v +
		  beta * clients * vt +
		  clients * cf
	# color scale represents gigabytes
	data$Z <- data$Z/1024^3

	pdf(paste("data_pior_caso_menor-b", beta * 100, ".pdf", sep = ""))
	print(levelplot(Z ~ X*Y, data=data , cuts = 100, xlab="Active clients", ylab="Providers", main=TeX(sprintf("Storaged data size(GB) - Worst Case (m < p) - $\\beta = %.1f$", beta)), col.regions = rainbow(100), panel = panel.levelplot.raster))
	dev.off()

	# # worst case (mix)
	m <- 500
	lower_m <- (providers * cp) + 
		  (clients * sr) +
		  clients * (((providers^2 + providers)/2 * sp) + providers * (cc)) +
		  beta * clients * vc +
		  beta * clients * (clients - 1) * v +
		  beta * clients * vt +
		  clients * cf
	upper_m <- (providers * cp) + 
		  clients * sr +
		  clients * (((2 * providers * m - m^2 -m)/2) * sp + m * cc) +
		  beta * clients * vc +
		  beta * clients * ((m * clients)/providers - 1) * v +
		  beta * clients * vt +
		  clients * cf
	threshold <- sum(providers < m)
	data$Z <- c(lower_m[1:threshold], upper_m[(threshold + 1):length(data$X)])

	# color scale represents terabytes
	data$Z <- data$Z/1024^4

	pdf(paste("data_pior_caso_mix-b", beta * 100, ".pdf", sep = ""))
	print(levelplot(Z ~ X*Y, data=data , cuts = 100, xlab="Active clients", ylab="Providers", main=TeX(sprintf("Storaged data size(GB) - Worst Case (m = 500) - $\\beta = %.1f$", beta)), col.regions = rainbow(100), panel = panel.levelplot.raster))
	dev.off()

}
