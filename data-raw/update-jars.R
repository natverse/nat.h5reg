# update jars

update_jars <- function(thdir="~/dev/Java/transform-helpers"){
  thdir=path.expand(thdir)
  try(git2r::pull(thdir))
  sha1=git2r::sha(git2r::last_commit(thdir))
  cmd <- sprintf('cd "%s" && mvn -Pfat clean package', thdir)
  rval=system(cmd)
  if(rval>0)
    stop("Failed to build transform-helpers!")
  jars=dir(file.path(thdir, 'target'), pattern = "shaded\\.jar$", full.names = T)
  if(!length(jars))
    stop("Couldn't find any jar files!")
  if(length(jars)>1)
    warning("Multiple jars. Choosing the last by alphanumeric sort order!")
  jar=rev(jars)[1]

  file.copy(jar, 'inst/java', overwrite = TRUE)
  message("Updated to transform-helpers: ", sha1)
}


