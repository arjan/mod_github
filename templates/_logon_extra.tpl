{% with
    "#000"
    as
    brand_color
%}
{% if m.config.mod_github.useauth.value and m.config.mod_github.appid.value %}
<li id="logon_github">
	{% if is_connect and 'github'|member:identity_types %}
		<a id="{{ #githubdis }}" href="#disconnect" class="btn z-btn-social" style="background-color: #000"><span class="z-icon z-icon-github"></span> {_ Disconnect from GitHub _}</a>
		{% wire id=#githubdis
				action={confirm title=_"Disconnect from GitHub"
								text=_"Do you want to disconnect your GitHub account?"
								ok=_"Disconnect"
								action={auth_disconnect id=m.acl.user type="github"}
						}
		%}
	{% else %}
		<a href="{% url logon_service service='github' is_connect=is_connect  %}" target="_blank" class="btn z-btn-social do_popupwindow" style="background-color: #000"><span class="z-icon z-icon-github"></span> {% if is_connect %}{_ Connect with GitHub _}{% else %}{_ Log in with GitHub _}{% endif %}</a>
	{% endif %}
</li>
{% endif %}
{% endwith %}
