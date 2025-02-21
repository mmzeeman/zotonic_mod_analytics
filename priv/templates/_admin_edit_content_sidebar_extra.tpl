{% if m.acl.is_allowed.use.mod_admin %}
    {% include "_admin_edit_sidebar_stats.tpl" id=id %}
{% endif %}
