{% wire id="admin_github" type="submit" postback="admin_github" delegate=`mod_github` %}
<form name="admin_github" id="admin_github" class="form-horizontal" method="POST" action="postback">
    <div class="row">
        <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header"><span class="fa fa-github"></span> GitHub</h3>
                <div class="widget-content">

                    <p class="help-block">
                        {_ You can find the application keys in _} <a href="https://github.com/settings/developers" title="GitHub Settings > OAuth applications" target="_blank">{_ GitHub Settings > OAuth applications _}</a>
                    </p>

                    <p class="help-block">
                        {_ The OAuth Authorization Callback URL is _}
                        <tt>{% url github_redirect use_absolute_url z_language=`undefined` %}</tt>
                    </p>

                    <div class="form-group row">
                        <label class="control-label col-md-3" for="github_appid">{_ Cient ID _}</label>
                        <div class="col-md-9">
                            <input type="text" id="github_appid" name="appid" value="{{ m.config.mod_github.appid.value|escape }}" class="form-control" />
                        </div>
                    </div>

                    <div class="form-group row">
                        <label class="control-label col-md-3" for="github_appsecret">{_ Client Secret _}</label>
                        <div class="col-md-9">
                            <input type="text" id="github_appsecret" name="appsecret" value="{{ m.config.mod_github.appsecret.value|escape }}" class="form-control" />
                        </div>
                    </div>

                    <div class="form-group row">
                        <div class="col-md-9 col-md-offset-3">
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox" id="github_useauth" name="useauth" {% if m.config.mod_github.useauth.value %}checked="checked"{% endif %} value="1" />
                                    {_ Use GitHub authentication _}
                                </label>
                            </div>
                        </div>
                    </div>

                    <div class="form-group row">
                        <div class="col-md-9 col-md-offset-3">
                            <button class="btn btn-primary" type="submit">{_ Save GitHub Settings _}</button>
                        </div>
                    </div>

                </div>
            </div>
        </div>
    </div>
</form>
