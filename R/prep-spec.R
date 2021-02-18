json_specification <- rjson::fromJSON(spec_json);
for (name in names(json_specification)){
    json_specification[[name]] = dplyr::as_tibble(json_specification[[name]]);
}
bt_specification <- json_specification;

