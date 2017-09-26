{% if 'github'|member:identity_types and m.config.mod_linkedin.useauth.value and m.config.mod_github.appid.value %}
<p>{_ You have coupled your <strong>GitHub</strong> account. _} <a href="{% url logon use_absolute_url%}">Log on with GitHub</a></p>
{% endif %}
