using UnityEngine;

public class Platforms : MonoBehaviour
{
    public Color enterColor;
    public Color notEnough;
    public GameObject turret;
    public GameObject buildEffect;
    private Renderer _rend;
    private Color _startColor;
    private BuildManager _buildManager;
    public void Start()
    {
        _rend = GetComponent<Renderer>();
        _startColor = _rend.material.color;
        _buildManager = BuildManager.instance;
    }
    public void OnMouseEnter()
    {
        if (!_buildManager.build)
        {
            return;
        }
        if (_buildManager.money)
        {
            _rend.material.color = enterColor;
        }
        else
            _rend.material.color = notEnough;
    }
    public void OnMouseExit()
    {
        _rend.material.color = _startColor;
    }
    public void OnMouseDown()
    {
        if (!_buildManager.build)
        {
            return;
        }
        if (turret != null)
        {
            return;
        }
        _buildManager.BuildTurret(this);
        Instantiate(buildEffect, transform.position, transform.rotation);
        AudioManager.instance.Play("Build");
    }
    public void SellTower()
    {

    }
}
