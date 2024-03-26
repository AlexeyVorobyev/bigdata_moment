transform_data_to_named_list <- function (data) {
  data_colnames <- colnames(data)

  data_options_colnames <- data_colnames[3:length(data_colnames)]

  options_named_list <- Map(
    function (colname) unlist(data[colname], use.names=FALSE),
    data_options_colnames
  )

  options_vectors <- Map(
    function (item) unlist(item, use.names=FALSE),
    options_named_list
  )

  options_vectors_exlude_ns <- Map(
    function (item) {
      return(Filter(
        function (item) item != 0,
        item
      ))
    }
    , options_vectors
  )

  return(options_vectors_exlude_ns)
}