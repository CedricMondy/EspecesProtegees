install_dependencies <- function(dep) {
    if (!require(pacman))
        install.packages("pacman")
    
    not_installed <- dep[!pacman::p_isinstalled(dep)]
    
    if (length(not_installed) > 0)
        pacman::p_install(not_installed, character.only = TRUE)
}
