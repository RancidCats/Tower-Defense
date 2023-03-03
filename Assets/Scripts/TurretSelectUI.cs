using UnityEngine;

public class TurretSelectUI : MonoBehaviour
{
    public GameObject selectUI;
    private Platforms _target;
    
    public void SetTarget(Platforms trgt)
    {
        _target = trgt;
        transform.position = _target.transform.position;
        selectUI.SetActive(true);
    }
    public void Hide()
    {
        selectUI.SetActive(false);
    }
}
